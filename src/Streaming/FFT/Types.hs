{-# LANGUAGE GADTs #-}

{-# OPTIONS_GHC -Wall #-}

module Streaming.FFT.Types
  ( 
    -- * FFT info 
    Info(..)

    -- * Accelerate interop types
  , AccWindow(..)
  , AccTransform(..)
  , AccSignal(..)
  , AccShift(..)
  , AccBin(..)

    -- * Haskell types
  , Signal(..)
  , Shift(..)
  , Bin(..)
  , Transform(..)
  , Window(..)
  , Threshold(..)

    -- * Debugging
  , undefined
  ) where

import Prelude hiding (undefined)
import Control.Monad.Primitive
import Data.Primitive.PrimArray
import Data.Array.Accelerate hiding (undefined, (++))
import Data.Array.Accelerate.Data.Complex

{-# WARNING undefined "'undefined' remains in code" #-}
undefined :: a
undefined = error "Prelude.undefined"

-- | information from FFT
data Info e where
  Empty     :: Info e
  Anomaly   :: Info e 
  Anomalies :: Int -> Info e
  Debug     :: e   -> Info e

instance Semigroup (Info e) where
  Empty <> Empty = Empty
  Empty <> Anomaly = Anomaly 
  Empty <> Anomalies x = Anomalies x
  Anomaly <> Empty = Anomaly
  Anomaly <> Anomaly = Anomalies 2
  Anomaly <> Anomalies y = Anomalies (y + 1)
  Anomalies x <> Anomaly = Anomalies (x + 1) 
  Anomalies x <> Empty = Anomalies x
  Anomalies x <> Anomalies y = Anomalies (x + y)
  Debug _ <> Debug _ = Empty
  Debug _ <> y = y
  x <> Debug _ = x
  {-# INLINE (<>) #-}

instance Show e => Show (Info e) where
  show Empty = "0 Anomalies"
  show Anomaly = "Anomaly detected"
  show (Anomalies x) = show x ++ " Anomalies detected"
  show (Debug e) = show e

-- | FIXME: doc
newtype AccWindow e = AccWindow
  { getAccWindow :: Acc (Vector (Complex e)) }

-- | FIXME: doc
newtype AccTransform e = AccTransform
  { getAccTransform :: Acc (Vector (Complex e)) }

newtype Window m e = Window
  { getWindow :: MutablePrimArray (PrimState m) (Complex e) }

newtype Transform m e = Transform
  { getTransform :: MutablePrimArray (PrimState m) (Complex e) }

newtype AccSignal e = AccSignal (Exp e)
newtype AccShift  e = AccShift  (Exp Int)
newtype AccBin    e = AccBin    (Exp Int)
newtype Signal e = Signal Int
newtype Shift  e = Shift  Int
newtype Bin    e = Bin    Int

newtype Threshold e = Threshold e

