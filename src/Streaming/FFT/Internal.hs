{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples       #-}

{-# OPTIONS_GHC -Wall -fwarn-redundant-constraints #-}

module Streaming.FFT.Internal
  ( initialDFT
  , subDFT

    -- * some stuff (???)
  , rToComplex
  , iToComplex 
  , mkComplex
  , getX, getY
  
  
  , updateWindow 
  , updateWindow'
  ) where

import Control.Applicative (Applicative(pure))
import Control.Monad.Primitive
import Data.Complex hiding (cis)
import Data.Function (($))
import Data.Functor (Functor(fmap))
import Data.Primitive.PrimArray
import Data.Primitive.Types
import GHC.Num (Num(..))
import GHC.Real
import GHC.Types (Int(..))
import Prelude ()
import Data.Primitive.Instances ()
import Streaming.FFT.Types
import qualified Data.Complex as C
import qualified Data.Primitive.Contiguous.FFT as CF
import qualified Prelude as P

cis :: P.Floating e
  => e
  -> e
  -> Complex e
cis k n = C.cis (2 * P.pi * k / n)
{-# INLINE cis #-}

getX :: Complex e -> e
getX (x :+ _) = x
{-# INLINE getX #-}

getY :: Complex e -> e
getY (_ :+ y) = y
{-# INLINE getY #-}

mkComplex :: e
          -> e
          -> Complex e
mkComplex x y = x :+ y
{-# INLINE mkComplex #-}

rToComplex :: P.Num e
           => e
           -> Complex e
rToComplex e = e :+ 0
{-# INLINE rToComplex #-}

iToComplex :: P.Num e
           => e
           -> Complex e
iToComplex e = 0 :+ e
{-# INLINE iToComplex #-}

initialDFT :: forall m e. (P.RealFloat e, Prim e, PrimMonad m)
  => Window m e
  -> m (Transform m e)
initialDFT (Window !w) = fmap Transform $ stToPrim $ CF.dftMutable w
{-# INLINE initialDFT #-}

-- | Compute FFT, F2, of a Window x2 given a new sample
--   and the Transform of the old sample x1,
--   
--   IN-PLACE. (F2 is a mutated F1)
--
--   /O(n)/
subDFT :: forall m e. (P.RealFloat e, Prim e, PrimMonad m)
       => Signal e   -- N
       -> Window m e -- x1
       -> Complex e
       -> Transform m e -- F1, given
       -> m (Transform m e) -- F2
subDFT (Signal n) (Window x1) x2_N_1 (Transform f1) = do
  let sz = P.fromIntegral n :: e
      l = sizeofMutablePrimArray f1 
  x1_0 <- readPrimArray x1 0 :: m (Complex e)
  let go :: Int -> m ()
      go ix = if (ix P.< l)
        then do
          f1_k <- readPrimArray f1 ix
          let foo' = cis (P.fromIntegral ix) sz
              res  = f1_k + x2_N_1 + x1_0
              fin  = foo' * res
          writePrimArray f1 ix fin
          go (ix + 1)
        else pure ()
  go 0
  pure $ Transform f1

updateWindow' :: forall m e. (Prim e, PrimMonad m, P.RealFloat e)
              => Window m e
              -> Complex e
              -> Int         -- ^ how many zeroed bins. for dense enough streams, this will be 0 most of the time
              -> m ()
updateWindow' (Window !mpa) !c !i = do
  let !sz = sizeofMutablePrimArray mpa
      !szm1 = sz - 1
      go :: Int -> m ()
      go !ix = if ix P.== szm1
        then do
          !_ <- writePrimArray mpa ix c
          P.return ()
        else if ix P.< szm1 P.&& (ix P.> szm1 P.- i)
          then do
            !_ <- writePrimArray mpa ix 0
            go (ix + 1)
          else do
            !x <- readPrimArray mpa ix
            !_ <- writePrimArray mpa (ix - 1) x
            go (ix + 1)
  go 1

updateWindow :: forall m e. (Prim e, PrimMonad m)
             => Window m e
             -> Complex e
             -> m ()
updateWindow (Window mpa1) c = do
  let !sz = sizeofMutablePrimArray mpa1
      !szm1 = sz - 1
      go :: Int -> m ()
      go !ix = if ix P.== szm1
          then do
            !_ <- writePrimArray mpa1 ix c
            P.return ()
          else do
            !x <- readPrimArray mpa1 ix
            !_ <- writePrimArray mpa1 (ix - 1) x
            go (ix + 1)
  go 1
