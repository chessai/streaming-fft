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
  , Ratio(..) 
  , Rational
  , Pair(..)
  , Triple(..)
  , Set(..)
  , singleton
    -- * Debugging
  , undefined
  ) where

import Prelude hiding (undefined, Rational)
import Control.Monad.Primitive
import Data.Primitive.PrimArray
import Data.Array.Accelerate hiding (undefined, (++))
import Data.Array.Accelerate.Data.Complex
import qualified Data.Set as Set

data Ratio a = R { numerator :: !a, denominator :: !a }
  deriving (Prelude.Eq, Prelude.Ord, Show)

type Rational = Ratio Integer

data Triple a b c = Triple !a !b !c
  deriving (Prelude.Eq, Prelude.Ord)


data Pair a b = Pair !a !b
  deriving (Prelude.Eq, Prelude.Ord)

newtype Set a = Set (Set.Set a)

instance Prelude.Ord a => Semigroup (Set a) where
  Set x <> Set y = Set (x <> y)

instance Show a => Show (Set a) where
  show (Set x) = show (Set.toList x)

singleton :: a -> Set a
singleton x = Set (Set.singleton x)
{-# INLINE singleton #-}

{-# WARNING undefined "'undefined' remains in code" #-}
undefined :: a
undefined = error "Prelude.undefined"

data Info e where
  Empty :: Info e
  Anomaly :: Triple (e, Int) (Set Int) Int -> Info e

instance Prelude.Ord e => Prelude.Semigroup (Info e) where
  Empty <> Empty = Empty
  Empty <> Anomaly x = Anomaly x
  Anomaly x <> Empty = Anomaly x
  Anomaly (Triple e1 s1 f1) <> Anomaly (Triple e2 s2 f2) = Anomaly (Triple (Prelude.max e1 e2) (s1 <> s2) (f1 + f2))

{-
data Info e where
  Empty   :: Info e
  Anomaly :: Pair Int Int -> Info e

instance Prelude.Semigroup (Info e) where
  Empty <> Empty = Empty 
  Empty <> Anomaly x = Anomaly x
  Anomaly x <> Empty  = Anomaly x
  Anomaly (Pair ix1 f1) <> Anomaly (Pair ix2 f2) = Anomaly $ Pair (Prelude.max ix1 ix2) (f1 + f2)
-}
instance Show e => Show (Info e) where
  show Empty                 =
    mconcat
      [ "---------------------------------"
      , "\n\n0 Anomalies detected.\n"
      ]
  show (Anomaly (Triple i s f)) =
    mconcat
      [ "---------------------------------"
      , "\nViolations: "
      , show f
      , "\nAnomalies detected at indeces: "
      , show s
      , "\nWorst offender at index: "
      , show i
      ]

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

newtype Threshold e = Threshold (Complex e)

