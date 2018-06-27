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
  , streamFFTDebug
  , binDepleted
  ) where

import Prelude
  ( RealFloat
  , Floating(..)
  , (**)
  , Int
  , mod
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
import Data.Primitive.Types
import Data.Semigroup (Semigroup((<>)))
import Data.String (String)
import GHC.Classes (modInt#)
import GHC.Num (Num(..))
import GHC.Real (fromIntegral, (/))
import GHC.Show (Show(..))
import GHC.Types (Int(..))
import Streaming.FFT.Internal.Accelerate (initialDFT, subDFT, updateWindow, mkComplex, getY)
import Streaming.FFT.Internal.Streaming
import Streaming.FFT.Types (Window(..), Transform(..), Signal(..), Info(..), Bin(..), Threshold(..))
import Streaming (lift)
import Streaming.Prelude (next, yield)
import qualified Streaming.FFT.Internal.Accelerate as SA

--import Streaming.FFT.Types (undefined)
import Debug.Trace

extract :: forall m e. (Ord e, RealFloat e, Prim e, PrimMonad m, Show e)
  => Threshold e
  -> Transform m e
  -> m (Info e)
extract (Threshold !t) (Transform !mpa) = do
  let !l = sizeofMutablePrimArray mpa 
      go :: Int -> m (Info e)
      go !ix = if ix < l
        then do
          !atIx@(s_r :+ s_i) <- readPrimArray mpa ix
          if s_i > t
            then fmap (Anomaly <>) (go (ix + 1))
            else go (ix + 1)
        else return Empty
  
--  !_ <- removeGaussian mpa
  go 0

removeGaussian :: forall m e. (RealFloat e, Prim e, PrimMonad m)
  => MutablePrimArray (PrimState m) (Complex e)
  -> m ()
removeGaussian !mpa = do
  !g_mean@(g_r :+ g_i) <- gmean mpa
  let !l = sizeofMutablePrimArray mpa
      go :: Int -> m ()
      go !ix = if ix < l
        then do
         !atIx@(s_r :+ s_i) <- readPrimArray mpa ix
         let !absd = (s_r - g_r) :+ abs (s_i - g_i)
         !_ <- writePrimArray mpa ix absd --(atIx - g_mean)
         go (ix + 1)
        else return ()
  go 0

amean :: forall m e. (Prim e, PrimMonad m, RealFloat e)
      => MutablePrimArray (PrimState m) (Complex e)
      -> m (Complex e)
amean !mpa = do
  let !l = sizeofMutablePrimArray mpa
      go :: Int -> Complex e -> m (Complex e)
      go !ix acc = if ix < l
        then do
          !atIx <- readPrimArray mpa ix
          go (ix + 1) (acc + atIx)
        else return acc
  !t <- go 0 0
 
  return (t / (fromIntegral l :: Complex e))

gmean :: forall m e. (Prim e, PrimMonad m, RealFloat e)
      => MutablePrimArray (PrimState m) (Complex e)
      -> m (Complex e)
gmean !mpa = do
  let !l = sizeofMutablePrimArray mpa
      go :: Int -> Complex e -> m (Complex e)
      go !ix acc = if ix < l
        then do
          !atIx <- readPrimArray mpa ix
          go (ix + 1) (acc * atIx)
        else return acc
  !t <- go 0 1
  
  let !p = mkComplex (fromIntegral l :: e) 0

  return (t ** (1 / p))

binDepleted :: forall e. (Num e, Ord e)
             => Bin e
             -> e -- First time value in current Bin
             -> e -- new time value
             -> Bool
binDepleted (Bin binSize) old new = new > old + fromIntegral binSize

streamFFTDebug :: forall m e b. (Prim e, SA.RealFloat e, SA.Elt (Complex e), SA.FromIntegral Int e, PrimMonad m, RealFloat e)
  => Threshold e -- ^ anomaly threshold
  -> Bin e       -- ^ bin size
  -> Signal e    -- ^ signal size
  -> Stream (Of e) m b -- ^ input stream
  -> Stream (Of String) m () -- ^ output stream

streamFFTDebug _ b@(Bin binSize) s@(Signal sigSize) strm = do
  mpaW :: MutablePrimArray (PrimState m) (Complex e) <- lift $ newPrimArray sigSize -- window mprimarray
  
  let loadInitial
        :: Int -- ^ index
        -> Int -- ^ bin accumulator
        -> e   -- ^ bin pivot 
        -> Int -- ^ have we finished consuming the signal
        -> Stream (Of e) m b
        -> Stream (Of String) m ()
      loadInitial !ix !binAccum !binFirst !untilSig st = if (untilSig >= sigSize) then lift $ return () else do
        e <- lift $ next st
        case e of
          Left _ -> lift $ return ()
          Right (x, rest) -> if ix == 0
            then loadInitial (ix + 1) binAccum x untilSig st
            else do
              let isDepleted = binDepleted b binFirst x
              if isDepleted
                then do
                  let !k = mkComplex (fromIntegral ix - 1) (fromIntegral binAccum) :: Complex e
                  !_ <- yield ((show k)  <> " " <> show isDepleted <> " " <> show binSize <> " " <> show binFirst)
                  lift $ writePrimArray mpaW (mod (ix - 1 + untilSig) sigSize) (mkComplex (fromIntegral ix - 1) (fromIntegral binAccum)) :: Stream (Of String) m ()
                  loadInitial (ix + 1) 0 x (untilSig + 1) rest
                else
                  loadInitial (ix + 1) (binAccum + 1) binFirst untilSig rest
 
  loadInitial 0 0 0 0 strm
  
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
      loadInitial !ix !binAccum !binFirst !untilSig st = if (untilSig >= sigSize) then return st else do
        e <- next st
        case e of
          Left _ -> return st
          Right (x, rest) -> if ix == 0
            then loadInitial (ix + 1) binAccum x untilSig st
            else do
              let isDepleted = binDepleted b binFirst x
              if isDepleted
                then do
                  let !k = mkComplex (fromIntegral ix - 1) (fromIntegral binAccum) :: Complex e
                  !_ <- writePrimArray mpaW (unsafeMod (ix - 1 + untilSig) sigSize) k :: m ()
                  loadInitial (ix + 1) 0 x (untilSig + 1) rest
                else
                  loadInitial (ix + 1) (binAccum + 1) binFirst untilSig rest

  subStrm :: Stream (Of e) m b <- lift $ loadInitial 0 0 0 0 strm
  
  !initialT <- lift $ initialDFT (Window mpaW)

  !initialInfo <- lift $ extract t initialT 
  
  !_ <- yield initialInfo

  let thereafter
        :: Int -- ^ index
        -> Int -- ^ have we filled a bin
        -> e   -- ^ first thing in the bin
        -> Window m e -- ^ window
        -> Transform m e -- ^ transform
        -> Stream (Of e) m b
        -> Stream (Of (Info e)) m b
      thereafter !ix !binAccum !binFirst win trans st = do
        e <- lift $ next st
        case e of
          Left r -> return r
          Right (x, rest) -> if ix == 0
            then thereafter (ix + 1) binAccum x win trans st
            else do
              let isDepleted = binDepleted b binFirst x
              if isDepleted
                then do
                  let k :: (Complex e)
                      !k = mkComplex (fromIntegral ix - 1) (fromIntegral binAccum)
                  !trans' <- lift $ subDFT s win k trans
                  !info <- lift $ extract t trans'
                  yield info
                  !_ <- lift $ updateWindow win k
                  thereafter (ix + 1) 0 x win trans' rest
                else thereafter (ix + 1) (binAccum + 1) binFirst win trans rest
 
  thereafter 0 0 0 (Window mpaW) initialT subStrm

unsafeMod :: Int -> Int -> Int
unsafeMod (I# x#) (I# y#) = I# (modInt# x# y#)
