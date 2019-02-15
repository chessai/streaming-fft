{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeInType #-}
{-# OPTIONS_GHC -Wall #-}

module Streaming.FFT.Types
  ( -- * types
    Signal(..)
  , Bin(..)
  , Transform(..)
  , Window(..)
  ) where

import Data.Kind (Type)
import Data.Primitive
import Control.Monad.Primitive
import Data.Complex
import Data.Primitive.Instances ()
import Prelude hiding (undefined, Rational)

-- | FIXME: Doc
newtype Window :: (Type -> Type) -> Type -> Type where
  Window :: MutablePrimArray (PrimState m) (Complex e) -> Window m e

-- | A type wrapping a 'MutablePrimArray'; it represents the DFT
--   on a window.
newtype Transform :: (Type -> Type) -> Type -> Type where
  Transform :: MutablePrimArray (PrimState m) (Complex e) -> Transform m e

foldMapTransform :: forall m a s. (Prim a, Monoid s, PrimMonad m) => (Complex a -> s) -> Transform m a -> m s
foldMapTransform f (Transform mpa) = do
  let !sz = sizeofMutablePrimArray mpa 
  let go :: Int -> s -> m s
      go ix !acc = if ix < sz
        then pure acc
        else do
          !x <- readPrimArray mpa ix
          go ix (mappend acc (f x))

  go 0 mempty

-- | Signal size
newtype Signal e = Signal Int

-- | Bin size
newtype Bin    e = Bin    Int
