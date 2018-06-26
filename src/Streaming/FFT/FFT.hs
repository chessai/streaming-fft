{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples       #-}

{-# OPTIONS_GHC -Wall #-}

module Streaming.FFT.FFT
  ( streamFFT
  ) where

import Prelude
  ( RealFloat
  , Int
  )

import Control.Monad (Monad(return))
import Control.Monad.Primitive
import Data.Bool (Bool(..))
import Data.Complex (Complex(..))
import Data.Either (Either(..))
import Data.Function (($))
import Data.Functor (Functor(fmap))
import Data.Ord (Ord(..))
import Data.Primitive.PrimArray
import Data.Primitive.Types
import Data.Semigroup (Semigroup((<>)))
import Data.Tuple (snd)
import GHC.Classes (modInt#)
import GHC.Num (Num(..))
import GHC.Real (fromIntegral, RealFrac(..), (/))
import GHC.Types (Int(..))
import Streaming.FFT.Internal.Accelerate (initialDFT, subDFT, updateWindow, mkComplex, getY)
import Streaming.FFT.Internal.Streaming
import Streaming.FFT.Types (Window(..), Transform(..), Signal(..), Info(..), Bin(..), Threshold(..))
--import Sonic.Types (undefined)
import Streaming (lift)
import Streaming.Prelude (next, yield)
import qualified Streaming.FFT.Internal.Accelerate as SA

-- | Extract information from the transform
extract :: forall m e. (Ord e, Prim e, PrimMonad m)
        => e -- ^ threshold for anomaly
        -> Transform m e
        -> m (Info e)
extract threshold (Transform !mpa) = do
  let !l = sizeofMutablePrimArray mpa
      go :: Int -> m (Info e) 
      go !ix = if ix < l
        then do
          !atIx <- readPrimArray mpa ix
          if (getY atIx) >= threshold
            then fmap (Anomalies 1 <>) (go (ix + 1))
            else fmap (Empty <>) (go (ix + 1))
        else return Empty

  go 0

binDepleted :: forall e. RealFrac e
             => Bin e
             -> e -- double whose whole part encodes how many bins in we are
             -> e
             -> (Bool, e)
binDepleted (Bin binSize) pivot new =
  let isDepleted = newVal >= 1.0
      newVal = (new / (fromIntegral binSize :: e)) + pivot
      newPivot = snd (properFraction newVal :: (Int, e))
  in (isDepleted, newPivot)

streamFFT :: forall m e b. (Num e, Prim e, SA.RealFloat e, SA.Elt (Complex e), SA.FromIntegral Int e, PrimMonad m, RealFloat e)
  => Threshold e
  -> Bin e
  -> Signal e
  -> Stream (Of e) m b
  -> Stream (Of (Info e)) m b
streamFFT (Threshold t) b s@(Signal sigSize) strm = do
  mpaW :: MutablePrimArray (PrimState m) (Complex e) <- lift $ newPrimArray sigSize -- window mprimarray

  let loadInitial
        :: Int -- ^ index
        -> Int -- ^ bin accumulator
        -> e   -- ^ bin pivot 
        -> Int -- ^ have we finished consuming the signal
        -> Stream (Of e) m b
        -> m (Stream (Of e) m b)
      loadInitial !ix !binAccum !binPivot !untilSig st = if (untilSig >= sigSize) then return st else do
        e <- next st
        case e of
          Left _ -> return st
          Right (x, rest) -> do
            let (isDepleted, newPivot) = binDepleted b binPivot x 
            if isDepleted
              then do
                writePrimArray mpaW (unsafeMod (ix + untilSig) sigSize) (mkComplex (fromIntegral ix) (fromIntegral binAccum)) :: m ()
                loadInitial (ix + 1)        0 newPivot (untilSig + 1) rest
              else
                loadInitial (ix + 1) binAccum newPivot  untilSig      rest
 
  subStrm :: Stream (Of e) m b <- lift $ loadInitial 0 0 0 0 strm
  
  initialT :: Transform m e <- lift $ initialDFT (Window mpaW)

  let thereafter
        :: Int -- ^ index
        -> Int -- ^ have we filled a bin
        -> e   -- ^ bin pivot
        -> Int -- ^ offset. currently unused
        -> Window m e -- ^ window
        -> Transform m e -- ^ transform
        -> Stream (Of e) m b
        -> Stream (Of (Info e)) m b
      thereafter !ix !binAccum !binPivot !offset win trans st = do
        e <- lift $ next st
        case e of
          Left r ->  return r
          Right (x, rest) -> do
            let (isDepleted, newPivot) = binDepleted b binPivot x
            if isDepleted
              then do
                let k :: (Complex e)
                    !k = mkComplex (fromIntegral ix) (fromIntegral binAccum)
                !trans' <- lift $ subDFT s win k trans
                !info <- lift $ extract t trans'
                yield info
                !_ <- lift $ updateWindow win k
                thereafter (ix + 1) 0 newPivot (offset + 1) win trans' rest
              else thereafter (ix + 1) (binAccum + 1) newPivot offset win trans rest
  
  thereafter 0 0 0 0 (Window mpaW) initialT subStrm

unsafeMod :: Int -> Int -> Int
unsafeMod (I# x#) (I# y#) = I# (modInt# x# y#)
