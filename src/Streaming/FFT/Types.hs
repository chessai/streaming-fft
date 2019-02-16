{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wall #-}

module Streaming.FFT.Types
  ( -- * types
    Transform(..)
  , Initial(..)
  , Window(..)
  ) where

-- | Initial bit
newtype Initial = Initial { getInitial :: forall s. MutablePrimArray s (Complex Double) }

-- | Represents the result of a transform of a 'Window'.
newtype Transform m = Transform { getTransform :: MutablePrimArray (PrimState m) (Complex Double) }

data Window
  = Window64
  | Window128
  | Window256
  | Window512
  | Window1024
