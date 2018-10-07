{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
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
import GHC.TypeLits
import Control.Monad.Primitive
import Data.Complex
import Data.Primitive.PrimArray
import Prelude hiding (undefined, Rational)

-- | FIXME: Doc
newtype Window :: (Type -> Type) -> Type -> Type where
  Window :: MutablePrimArray (PrimState m) (Complex e) -> Window m e

-- | FIXME: Doc
newtype Transform :: (Type -> Type) -> Type -> Type where
  Transform :: MutablePrimArray (PrimState m) (Complex e) -> Transform m e

-- | FIXME: Doc
data Signal' (n :: Nat) = Signal'

-- | FIXME: Doc
data Bin' (n :: Nat) = Bin'

newtype Signal e = Signal Int
newtype Bin    e = Bin    Int
