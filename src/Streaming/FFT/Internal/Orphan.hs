{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples       #-}

module Streaming.FFT.Internal.Orphan () where

import Control.Applicative
import Control.Monad.Primitive
import Control.Monad.ST
import Data.Complex
import Data.Primitive.Addr
import Data.Primitive.ByteArray
import Data.Primitive.PrimArray
import Data.Primitive.Types
import GHC.Prim
import GHC.Types

import Streaming.Internal (Stream(Return,Effect,Step))
import Streaming.Prelude (Of((:>)))
import qualified Streaming.Prelude as SMP
import qualified Streaming.Prelude as S

instance Prim e => Prim (Complex e) where
  sizeOf# _ = 2# *# sizeOf# (undefined :: e)
  alignment# _ = alignment# (undefined :: e)
  indexByteArray# arr# i# =
    let i = I# i#
        arr = ByteArray arr#
    in
         (indexByteArray arr (2 * i + 0))
      :+ (indexByteArray arr (2 * i + 1))
  readByteArray# :: forall s e. (Prim e) => MutableByteArray# s -> Int# -> State# s -> (# State# s, Complex e #)
  readByteArray# arr# i# = internal $ do
    let i = I# i#
        arr = MutableByteArray arr#
    a <- readByteArray arr (2 * i + 0) :: ST s e
    b <- readByteArray arr (2 * i + 1)
    return (a :+ b)
  writeByteArray# :: forall s e. (Prim e) => MutableByteArray# s -> Int# -> Complex e -> State# s -> State# s
  writeByteArray# arr# i# (a :+ b) = internal_ $ do
    let i = I# i#
        arr = MutableByteArray arr#
    writeByteArray arr (2 * i + 0) a
    writeByteArray arr (2 * i + 1) b :: ST s ()
  setByteArray# = defaultSetByteArray#
  indexOffAddr# :: Addr# -> Int# -> Complex e
  indexOffAddr# addr# i# =
    let i = I# i#
        addr = Addr addr#
    in (indexOffAddr addr (2 * i + 0)) :+ (indexOffAddr addr (2 * i + 1))
  readOffAddr# :: forall s e. (Prim e) => Addr# -> Int# -> State# s -> (# State# s, Complex e #)
  readOffAddr# addr# i# = internal $ do
    let i = I# i#
        addr = Addr addr#
    a <- readOffAddr addr (2 * i + 0) :: ST s e
    b <- readOffAddr addr (2 * i + 1)
    return (a :+ b)
  writeOffAddr# :: forall s e. (Prim e) => Addr# -> Int# -> Complex e -> State# s -> State# s
  writeOffAddr# addr# i# (a :+ b) = internal_ $ do
    let i = I# i#
        addr = Addr addr#
    writeOffAddr addr (2 * i + 0) a
    writeOffAddr addr (2 * i + 1) b :: ST s ()
  setOffAddr# = defaultSetOffAddr#

internal_ :: PrimBase m => m () -> State# (PrimState m) -> State# (PrimState m)
internal_ m s = case internal m s of
  (# s', () #) -> s'
