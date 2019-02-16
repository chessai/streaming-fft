{-# language BangPatterns        #-}
{-# language LambdaCase          #-}
{-# language MagicHash           #-}
{-# language ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wall #-}

module Streaming.FFT
  ( -- * streaming fft
    streamFFT
    
    -- * Types
  , Transform(..)
  , Window(..)
  ) where

import GHC.Classes (modInt#)
import Streaming.FFT.Types
import qualified Data.Complex as Complex
import qualified Data.Primitive.Contiguous as Contiguous
import qualified Data.Primitive.Contiguous.FFT as Contiguous
import qualified Streaming.Prelude as S
import Control.Monad.Trans.Class (lift)

intToDouble :: Int -> Double
{-# inline intToDouble #-}
intToDouble = fromIntegral

cissy :: Double -> Double -> Complex Double
cissy k n = Complex.cis (2 * pi * k / n)

windowSize :: Window -> Int
windowSize = \case
  Window64 -> 64
  Window128 -> 128
  Window256 -> 256
  Window512 -> 512
  Window1024 -> 1024

-- | Only safe when the second argument is not 0
unsafeMod :: Int -> Int -> Int
unsafeMod (I# x#) (I# y#) = I# (modInt# x# y#)
{-# inline unsafeMod #-} -- this should happen anyway. trust but verify.

-- | Compute the FFT of a previously computed FFT given a new sample.
--   This operation is done in-place.
--
--   /O(n)/
subFFT :: forall m. PrimMonad m
  => Complex Double -- ^ newest signal, x_{k+N}
  -> Transform m
  -> m ()
subFFT x_k_plus_N (Transform f1) = do
  n <- Contiguous.sizeMutable f1
  x_k <- Contiguous.read f1 0
  let go :: Int -> m ()
      go !f = if f < n
        then do {
            atIx <- Contiguous.read f1 f
          ; let expTerm = cissy (intToDouble $ (n + 1) * f) (intToDouble n)
                res = expTerm * (atIx + x_k_plus_N - x_k)
          ; Contiguous.write f1 f res
          ; go (f + 1) 
        }
        else pure ()
  go 0

loadInitial :: forall m b. (PrimMonad m)
  => MutablePrimArray (PrimState m) (Complex Double)
  -> Window -- ^ window size
  -> Int -- ^ index
  -> Int -- ^ have we finished consuming the signal
  -> Stream (Of (Complex Double)) m b -- ^ first part of stream
  -> m (Stream (Of (Complex Double)) m b) -- ^ stream minus original signal
loadInitial marr sig !ix !untilSig stream =
  if (untilSig >= windowSize sig)
    then pure stream
    else do {
      S.next stream >>= \case
        Left _ -> pure stream
        Right (x,rest) -> if ix == 0
          then loadInitial marr sig (ix + 1) untilSig stream
          else do {
              Contiguous.write marr (unsafeMod (ix - 1 + untilSig) (windowSize sig)) x
            ; loadInitial marr sig (ix + 1) (untilSig + 1) rest
          } 
    }

thereafter :: forall m b c. (PrimMonad m)
  => (Transform m -> m c)
  -> Window
  -> Int -- ^ have we filled the window size
  -> Transform m -- ^ transform
  -> Stream (Of (Complex Double)) m b
  -> Stream (Of c) m b
thereafter extract win !untilWin trans st =
  if (untilWin >= windowSize win)
    then thereafter extract win 0 trans st
    else do {
        e <- lift $ S.next st
      ; case e of {
            Left r -> pure r
          ; Right (x,rest) -> do {
                lift $ subFFT x trans
              ; info <- lift $ extract trans
              ; S.yield info
              ; thereafter extract win (untilWin + 1) trans rest
            }
        }
    }

streamFFT :: forall m b c. (PrimMonad m)
  => (Transform m -> m c)
  -> Window
  -> Stream (Of (Complex Double)) m b
  -> Stream (Of c) m b
streamFFT extract win stream = do
  -- Allocate the one array 
  marr <- lift $ Contiguous.new (windowSize win)

  -- Grab the first signal from the stream
  streamMinusFirst <- lift $ loadInitial marr win 0 0 stream
  
  -- Get our first transform
  initialT :: Transform m <- lift $ do { Contiguous.mfft marr; pure $ Transform marr }

  -- Extract information from that transform
  initialInfo <- lift $ extract initialT

  -- Yield that information to the new stream
  S.yield initialInfo

  -- Now go
  thereafter extract win 0 initialT streamMinusFirst
