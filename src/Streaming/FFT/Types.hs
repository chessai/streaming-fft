{-# LANGUAGE GADTs #-}

{-# OPTIONS_GHC -Wall #-}

module Streaming.FFT.Types
  ( -- * types
    Signal(..)
  , Shift(..)
  , Bin(..)
  , Transform(..)
  , Window(..)
  ) where

import Control.Monad.Primitive
import Data.Complex
import Data.Primitive.PrimArray
import Prelude hiding (undefined, Rational)

-- {-# WARNING undefined "'undefined' remains in code" #-}
-- undefined :: a
-- undefined = error "Prelude.undefined"

newtype Window m e = Window
  { getWindow :: MutablePrimArray (PrimState m) (Complex e) }

newtype Transform m e = Transform
  { getTransform :: MutablePrimArray (PrimState m) (Complex e) }

newtype Signal e = Signal Int
newtype Shift  e = Shift  Int
newtype Bin    e = Bin    Int
