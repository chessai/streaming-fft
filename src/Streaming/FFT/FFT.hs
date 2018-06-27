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
  , Floating
  , (**)
  , Int
  )

import Control.Monad (Monad(return))
import Control.Monad.Primitive
import Data.Bool (Bool(..))
import Data.Complex (Complex(..))
import Data.Either (Either(..))
import Data.Eq (Eq((==)))
import Data.Function (($))
import Data.Functor (Functor(fmap))
import Data.Ord (Ord(..))
import Data.Primitive.PrimArray
import Data.Primitive.PrimArray.Median (median)
import Data.Primitive.Types
import Data.Semigroup (Semigroup((<>)))
import Data.Tuple (snd)
import GHC.Classes (modInt#)
import GHC.Num (Num(..))
import GHC.Real (fromIntegral, RealFrac(..), Fractional(..), (/))
import GHC.Types (Int(..))
import Streaming.FFT.Internal.Accelerate (initialDFT, subDFT, updateWindow, mkComplex, getY)
import Streaming.FFT.Internal.Streaming
import Streaming.FFT.Types (Window(..), Transform(..), Signal(..), Info(..), Bin(..), Threshold(..))
import Streaming.FFT.Types (undefined)
import Streaming (lift)
import Streaming.Prelude (next, yield)
import qualified Streaming.FFT.Internal.Accelerate as SA

extract :: forall m e. (Ord e, RealFloat e, Prim e, PrimMonad m)
  => Threshold e
  -> Transform m e
  -> m (Info e)
extract (Threshold !t) (Transform !mpa) = do
  let !l = sizeofMutablePrimArray mpa
      go :: Int -> m (Info e)
      go !ix = if ix < l
        then do
          !atIx <- readPrimArray mpa ix
          if getY atIx > t
            then fmap (Anomaly <>) (go (ix + 1))
            else return Empty
        else return Empty

  !_ <- reduceNoise mpa
  go 0

reduceNoise :: forall m e. (Floating e, Prim e, PrimMonad m)
  => MutablePrimArray (PrimState m) e
  -> m ()
reduceNoise !mpa = do
  !g_mean <- gmean mpa

  let !l = sizeofMutablePrimArray mpa
      go :: Int -> m ()
      go !ix = if ix < l
        then do
         !atIx <- readPrimArray mpa ix
         !_ <- writePrimArray mpa ix (abs (atIx - g_mean))
         go (ix + 1)
        else return ()
  go 0

gmean :: forall m e. (Prim e, PrimMonad m, Floating e)
      => MutablePrimArray (PrimState m) e
      -> m e
gmean !mpa = do
  let !l = sizeofMutablePrimArray mpa
      go :: Int -> e -> m e
      go !ix acc = if ix < l
        then do
          !atIx <- readPrimArray mpa ix
          go (ix + 1) (acc * atIx)
        else return acc
  !t <- go 0 1
  let !p = 1 / (fromIntegral l :: e)

  return (t ** p)

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
  => Threshold e -- ^ anomaly threshold
  -> Bin e       -- ^ bin size
  -> Signal e    -- ^ signal size
  -> Stream (Of e) m b -- ^ input stream
  -> Stream (Of (Info e)) m b -- ^ output stream
streamFFT t b s@(Signal sigSize) strm = do
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
                loadInitial (ix + 1) 0 newPivot (untilSig + 1) rest
              else
                loadInitial (ix + 1) binAccum newPivot  untilSig      rest
 
  subStrm :: Stream (Of e) m b <- lift $ loadInitial 0 0 0 0 strm
  
  !initialT <- lift $ initialDFT (Window mpaW)

  !initialInfo <- lift $ extract t initialT 
  
  !_ <- yield initialInfo

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
                thereafter 0 0 newPivot (offset + 1) win trans' rest
              else thereafter (ix + 1) (binAccum + 1) newPivot offset win trans rest
  
  thereafter 0 0 0 0 (Window mpaW) initialT subStrm

unsafeMod :: Int -> Int -> Int
unsafeMod (I# x#) (I# y#) = I# (modInt# x# y#)
