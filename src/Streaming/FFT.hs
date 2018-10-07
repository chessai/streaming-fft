{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wall #-}

module Streaming.FFT
  ( streamFFT
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

{-# INLINABLE streamFFT #-}
streamFFT :: forall m e b c. (Prim e, PrimMonad m, RealFloat e)
  => (Transform m e -> m c) -- ^ extraction method
  -> Bin e       -- ^ bin size
  -> Signal e    -- ^ signal size
  -> Stream (Of e) m b -- ^ input stream
  -> Stream (Of c) m b -- ^ output stream
streamFFT extract b s@(Signal sigSize) strm = do
  -- Allocate the one array 
  mpaW :: MutablePrimArray (PrimState m) (Complex e) <- lift $ newPrimArray sigSize
  let win = Window mpaW
  
  -- Grab the first signal from the stream
  subStrm :: Stream (Of e) m b <- lift $ loadInitial mpaW b s 0 0 0 0 strm
 
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

