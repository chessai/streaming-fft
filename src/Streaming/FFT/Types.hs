{-# LANGUAGE GADTSyntax #-}
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
import Control.Monad.Primitive
import Data.Complex
import Data.Primitive.PrimArray
import Prelude hiding (undefined, Rational)

-- | A 'Window' is a mutable primitive array of 'Complex' values,
--   over which we compute the DFT. 
newtype Window :: (Type -> Type) -> Type -> Type where
  Window :: MutablePrimArray (PrimState m) (Complex e) -> Window m e

-- | A 'Transform' is a Mutable primitive array of 'Complex' values,
--   the result of taking the DFT of a 'Window'.
newtype Transform :: (Type -> Type) -> Type -> Type where
  Transform :: MutablePrimArray (PrimState m) (Complex e) -> Transform m e

-- | Your signal size.
newtype Signal e = Signal Int
-- | Your bin size.
newtype Bin    e = Bin    Int
