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

slidingBounds' :: forall a m b. (Prim a, PrimMonad m)
  => Int
  -> Stream (Of a) m b
  -> Stream (Of (MutablePrimArray (PrimState m) a)) m b
slidingBounds' !n' stream = Effect $ do
  let !n = max n' 1
  marr <- newPrimArray n 
  let go1 :: Int -> Stream (Of a) m b -> Stream (Of (MutablePrimArray (PrimState m) a)) m b
      go1 !ix s1 = case s1 of
        Return r -> Return r
        Effect m -> Effect (fmap (go1 ix) m)
        Step (a :> s2) -> if ix < n - 1
          then Effect (writePrimArray marr ix a >> return (go1 (ix + 1) s2))
          else Effect (writePrimArray marr ix a >> return (go2 0 s2))
      go2 :: Int -> Stream (Of a) m b -> Stream (Of (MutablePrimArray (PrimState m) a)) m b
      go2 !ix s1 = case s1 of
        Return r -> Return r
        Effect m -> Effect (fmap (go2 ix) m)
        Step (new :> s2) -> Effect $ do
          !old <- readPrimArray marr ix
          writePrimArray marr ix new
          return (Step (marr :> go2 (rem (ix + 1) n) s2))

  return (go1 0 stream)
  
 
