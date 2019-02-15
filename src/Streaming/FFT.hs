{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wall #-}

module Streaming.FFT
  ( -- * streaming fft
    streamFFT
    -- * Types
  , Transform(..)
  , Bin(..)
  , Signal(..)
  ) where

import Prelude
  ( RealFloat
  )

import Control.Monad (Monad(return))
import Control.Monad.Primitive
import Data.Complex (Complex(..))
import Data.Either (Either(..))
import Data.Eq (Eq((==)))
import Data.Function (($))
import Data.Ord (Ord(..))
import Data.Primitive.PrimArray
import Data.Primitive.Types
import GHC.Classes (modInt#)
import GHC.Num (Num(..))
import GHC.Real (fromIntegral, RealFrac(..))
import GHC.Types (Int(..))
import Streaming.FFT.Internal (initialDFT, subDFT, updateWindow', rToComplex)
import Streaming.FFT.Types (Window(..), Transform(..), Signal(..), Bin(..))
import Streaming
import Streaming.Prelude (next, yield)

data Depleted
  = NotDepleted -- ^ bin is not depleted
  | Past !Int   -- ^ how many bins we have past

binDepleted :: forall e. (Num e, Ord e, RealFrac e)
             => Bin e
             -> e
             -> e
             -> Depleted
binDepleted (Bin binSize) old new =
  let !k = new - (old + fromIntegral binSize)
  in if k > 0
    then Past (floor k)
    else NotDepleted

-- [NOTE]: A drawback of the dense-stream optimisation
-- is that we must keep track of the number of bins that
-- we ingest that are 0. if too many are 0 w.r.t. the signal
-- size, then we must fall back to the /O(n log n) computation
-- until we reach another dense area of the stream. This amounts
-- to keeping an Int around that counts the number of bins that
-- were equal to zero, it gets incremented after each bin is finished
-- loading. So, there should realy be two 'thereafter' functions,
-- and 'loadInitial' should do some additional checks.
-- This is currently not the case.
loadInitial :: forall m e b. (Prim e, PrimMonad m, RealFloat e)
  => MutablePrimArray (PrimState m) (Complex e) -- ^ array to which we should allocate
  -> Bin e -- ^ bin size
  -> Signal e -- ^ signal size
  -> Int -- ^ index
  -> Int -- ^ bin accumulator
  -> e   -- ^ bin pivot
  -> Int -- ^ have we finished consuming the signal
  -> Stream (Of e) m b -- first part of stream
  -> m (Stream (Of e) m b) -- stream minus original signal
loadInitial !mpa !b s@(Signal !sigSize) !ix !binAccum !binFirst !untilSig st = if (untilSig >= sigSize) then return st else do
  e <- next st
  case e of
    Left _ -> return st
    Right (x, rest) -> if ix == 0
      then loadInitial mpa b s (ix + 1) binAccum x untilSig st
      else do
        let isDepleted = binDepleted b binFirst x 
        case isDepleted of
          NotDepleted -> loadInitial mpa b s ix (binAccum + 1) binFirst untilSig rest
          Past i -> do
            let !k = rToComplex (fromIntegral binAccum) :: Complex e
            !_ <- writePrimArray mpa (unsafeMod (ix - 1 + untilSig) sigSize) k :: m ()
            loadInitial mpa b s (ix + i) 0 x (untilSig + 1) rest
{-# NOINLINE loadInitial #-}
-- loadInitial is recursive so it won't inline anyway, but it's better to be
-- explicit about it.

thereafter :: forall m e b c. (Prim e, PrimMonad m, RealFloat e)
  => (Transform m e -> m c) -- ^ extract
  -> Bin e -- ^ bin size
  -> Signal e -- ^ signal size
  -> Int -- ^ index
  -> Int -- ^ have we filled a bin
  -> e   -- ^ first thing in the bin
  -> Window m e -- ^ window
  -> Transform m e -- ^ transform
  -> Stream (Of e) m b
  -> Stream (Of c) m b
thereafter extract !b !s !ix !binAccum !binFirst win trans st = do
  e <- lift $ next st
  case e of
    Left r -> return r
    Right (x, rest) -> if ix == 0
      then thereafter extract b s (ix + 1) binAccum x win trans st
      else do
        let isDepleted = binDepleted b binFirst x
        case isDepleted of
          NotDepleted -> thereafter extract b s ix (binAccum + 1) binFirst win trans rest
          Past i -> do
            let k :: Complex e
                !k = rToComplex (fromIntegral binAccum)
            !trans' <- lift $ subDFT s win k trans
            !info <- lift $ extract trans'
            yield info
            -- a problem is that if too many empty bins pass,
            -- the optimised streaming-fft algorithm fails, and we
            -- need to revert (temporarily) to the original O(n log n)
            -- algorithm.
            !_ <- lift $ updateWindow' win k i
            thereafter extract b s (ix + i) 0 x win trans' rest
{-# NOINLINE thereafter #-}
-- thereafter is recursive so it won't inline anyway, but it's better to be
-- explicit about it.

-- | 'streamFFT' is based off ideas from signal processing, with an optimisation
--   outlined in <https://www.dsprelated.com/showarticle/776.php this blog post>.
--   Here, I will give you an outline of how this works. The idea is that we
--   have a stream of data, which we will divide into 'Signal's, and each 'Signal'
--   is something for which we want to compute the DFT. Each signal is divided into
--   'Bin's (more on this later, but you can just think of 'Bin's as a chunk of a
--   'Signal', where all the chunks are of equal length). We treat our stream not as
--   contiguous blocks of 'Signal's, but as overlapping 'Signal's, where each overlap
--   is one 'Bin'-length. The motivation for the blog post is to reduce the work of
--   this overlap; they show a way to compute the DFT of each 'Signal' subsequent
--   to the initial in /O(n)/ time, instead of the typical /O(n log n)/ time,
--   by abusing the overlap.
--  
--   Consider you would like to compute the Fourier Transform of the signal
--   
--     \[
--     x_{i-n+1}, x_{i-n+2}, ..., x_{i-1}, x_{i}.
--     \]
--  
--   However this means that when you receive \( x_{i+1} \), you'll be the computing
--   the Fourier Transform of
--   
--     \[
--     x_{i-n+2}, x_{i-n+3}, ..., x_{i}, x_{i+1},
--     \]
--   
--   which is almost identical to the first sequence. How do we avoid extra work?
--
--   Assume data windows to be of length \( N \) (this corresponds to the number of
--   'Bin's in the 'Signal'). Let
--   the original data window be \( x_{1} \), whose first sample is \( x_{old} = x_{1}[0] \).
--   (here, \( a[k] \) is used to denote accessing the \( (k-1)th \) element from
--   a sequence \( a \) ). Let your new data window be denoted as \( x_{2} \), whose
--   bins are one left-shifted version of \( x_{1} \), i.e.
--   \( x_{2}[k] = x_{1}[k+1]\) for \(k = 0, 1, ... N - 2 \), plus a new arrived datum to
--   position \( N - 1 \), which is denoted as \( x_{new} = x_{2}[N - 1]\).
--
--   The following will compute the N-point DFT, \( X_{2} \) of the new data set
--   \( x_{2} \) from that of the already computed and stored N-point DFT
--   \( X_{1} \) of the old data set \( x_{1} \):
--
--     \[
--     X{2}[k] = e^{2 \pi i k / N} * (X{1}[k] + (x_{new} - x_{old}))
--     \]
--
--  for each \( k = 0, 1, ..., N - 1 \). This updated computation of \( X{2} \)
--  pre-computed \( X{1} \) requires \( N \) complex multiplications and \( N \)
--  real additions. Compared to a direct N-point DFT which requires \( N log_{2}(N) \)
--  complex multiply-accumulate operations, this is an improvement by a factor of
--  \( log_{2}(N) \), which for example at N=1024 would translate to a speedup of
--  about 10.
--
--  Another advantage of this algorithm as this it is amenable to being done in-place.
--  `streamFFT` in fact does do this, and for that reason allocations are kept to an
--  absolute minimum.
--
--
streamFFT :: forall m a b c. (Prim a, PrimMonad m, RealFloat a)
  => (Transform m a -> m c) -- ^ extraction method. This is a function that takes a 'Transform'
                            --   and produces (or 'extracts') some value from it. It is used
                            --   to produce the values in the output stream.
  -> Bin a       -- ^ bin size
  -> Signal a    -- ^ signal size
  -> Stream (Of a) m b -- ^ input stream
  -> Stream (Of c) m b -- ^ output stream
{-# INLINABLE streamFFT #-}
streamFFT extract b s@(Signal sigSize) strm = do
  -- Allocate the one array 
  mpaW <- lift $ newPrimArray sigSize
  let win = Window mpaW
  
  -- Grab the first signal from the stream
  subStrm <- lift $ loadInitial mpaW b s 0 0 0 0 strm
 
  -- Compute the transform on the signal we just grabbed
  -- so we can perform our dense-stream optimisation
  !initialT <- lift $ initialDFT win

  -- Extract information from that transform
  !initialInfo <- lift $ extract initialT
 
  -- Yield that information to the new stream
  !_ <- yield initialInfo

  -- Now go
  thereafter extract b s 0 0 0 win initialT subStrm

-- | Only safe when the second argument is not 0
unsafeMod :: Int -> Int -> Int
unsafeMod (I# x#) (I# y#) = I# (modInt# x# y#)
{-# INLINE unsafeMod #-} -- this should happen anyway. trust but verify.
