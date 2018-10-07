{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Streaming.FFT.Nat
  ( natToInt
  ) where

import Data.Proxy (Proxy(Proxy))
import Data.TypeLits (KnowNat, natVal)

natToInt :: forall n. KnownNat n => Int
{-# INLINE natToInt #-}
natToInt = fromIntegral (natVal @n Proxy)

