{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples       #-}

{-# OPTIONS_GHC -Wall -fwarn-redundant-constraints #-}

module Streaming.FFT.Internal.Accelerate
  ( -- * DFT and IDFT 
    dft
  , idft
  
  , initialDFT
  , subDFT

    -- * Accelerate typeclasses
  , RealFloat(..)
  , FromIntegral(..)
  , Elt

    -- * some stuff (???)
  , rToComplex
  , mkComplex
  , getY
  --, rToExpComplex
  , modifyMutablePrimArray 
  , updateWindow 
  , updateWindow'
  ) where

import Prelude ()

import Control.Applicative (Applicative(pure))
import Control.Monad.Primitive
import qualified Data.Complex as C
import Data.Primitive.Types
import Data.Primitive.PrimArray
import Data.Array.Accelerate
import Data.Array.Accelerate.Data.Complex
import Data.Array.Accelerate.Interpreter
import qualified Data.Array.Accelerate.Math.DFT as DFT

import qualified Prelude as P
import Streaming.FFT.Types
import Streaming.FFT.Internal.Orphan ()

-- | monomorphised (!!)
readVector :: Elt e => Acc (Vector e) -> Exp Int -> Exp e
readVector = (!!)

runExp :: Elt e => Exp e -> e
runExp e = indexArray (run (unit e)) Z

myCis :: (P.Floating e)
      => e
      -> e
      -> Complex e
myCis k n = C.cis (2 * P.pi * k / n)
{-# INLINE myCis #-}

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

--rToExpComplex :: (Elt (Complex e), P.Num e)
--              => e
--              -> Exp (Complex e)
--rToExpComplex e = constant (e :+ 0)

-- | Convert a vector in the accelerate language
--   to a 'MutablePrimArray' in Haskell
vecToMut :: forall m a. (Elt a, Prim a, PrimMonad m)
         => Acc (Vector a)
         -> m (MutablePrimArray (PrimState m) a)
vecToMut accV = do
  let sz = runExp $ length accV
  mpa <- newPrimArray sz :: m (MutablePrimArray (PrimState m) a)
  
  let go :: Int -> m (MutablePrimArray (PrimState m) a)
      go ix = if (ix P.< sz)
        then do
          let atIx = runExp $ readVector accV (constant ix)
          writePrimArray mpa ix atIx
          go (ix + 1) 
        else P.return mpa
  go 0

mutToList :: forall m a. (Prim a, PrimMonad m)
          => MutablePrimArray (PrimState m) a
          -> m [a]
mutToList !mpa = do
  let !l = sizeofMutablePrimArray mpa
      go !ix xs = if ix P.< l
        then do
          !atIx <- readPrimArray mpa ix
          go (ix + 1) (atIx : xs)
        else 
          P.return xs
  go 0 []

-- | Compute the initial DFT
initialDFT :: forall m e. (Prim e, Elt (Complex e), RealFloat e, FromIntegral Int e, PrimMonad m)
           => Window m e
           -> m (Transform m e)
initialDFT (Window w) = do
  mutAcc <- mutToList w 
  let !l = P.length mutAcc 
      acc = lift $ fromList (Z:.l) mutAcc    

  let t = getAccTransform $ dft $ AccWindow acc
  mut <- vecToMut t
  pure $ Transform mut

-- | Compute FFT of a Window x2 given a new sample
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
          let foo' = myCis (P.fromIntegral ix) sz
              res  = f1_k + x2_N_1 + x1_0
              fin  = foo' * res
          writePrimArray f1 ix fin
          go (ix + 1)
        else pure ()
  go 0
  pure $ Transform f1

modifyMutablePrimArray :: forall m a. (Prim a, PrimMonad m)
                       => (a -> a)
                       -> MutablePrimArray (PrimState m) a
                       -> m ()
modifyMutablePrimArray f !mpa = do
  let !sz = sizeofMutablePrimArray mpa 
      go :: Int -> m ()
      go !ix = if ix P.< sz
        then do
          !x <- readPrimArray mpa ix
          !_ <- writePrimArray mpa ix (f P.$! x)
          go (ix + 1)
        else P.return ()
  go 0

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

-- | DFT
dft :: (Elt (Complex e), RealFloat e, FromIntegral Int e)
    => AccWindow e
    -> AccTransform e
dft (AccWindow v) = AccTransform (DFT.dft v)

-- | Inverse DFT
idft :: (Elt (Complex e), RealFloat e, FromIntegral Int e)
     => AccTransform e
     -> AccWindow e
idft (AccTransform v) = AccWindow (DFT.idft v)
