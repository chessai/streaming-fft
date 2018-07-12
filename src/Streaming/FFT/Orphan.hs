{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples       #-}

{-# OPTIONS_GHC
      -Wall
      -fno-warn-orphans
      -fno-warn-name-shadowing
  #-}

module Streaming.FFT.Orphan () where

import Control.Monad.Primitive
import Control.Monad.ST
import Data.Complex
import Data.Primitive.Addr
import Data.Primitive.ByteArray
import Data.Primitive.Types
import GHC.Prim
import GHC.Types

instance Prim a => Prim (Complex a) where
  sizeOf# _ = 2# *# sizeOf# (undefined :: a)
  alignment# _ = alignment# (undefined :: a)
  indexByteArray# arr# i# =
    let x = indexByteArray# arr# (2# *# i#)
        y = indexByteArray# arr# (2# *# i# +# 1#)
    in x :+ y
  readByteArray# arr# i# =
    \s0 -> case readByteArray# arr# (2# *# i#) s0 of
       (# s1#, x #) -> case readByteArray# arr# (2# *# i# +# 1#) s1# of
          (# s2#, y #) -> (# s2#, x :+ y #)
  writeByteArray# arr# i# (a :+ b) =
    \s0 -> case writeByteArray# arr# (2# *# i#) a s0 of
       s1 -> case writeByteArray# arr# (2# *# i# +# 1#) b s1 of
         s2 -> s2
  setByteArray# = defaultSetByteArray#
  indexOffAddr# addr# i# =
    let x = indexOffAddr# addr# (2# *# i#)
        y = indexOffAddr# addr# (2# *# i# +# 1#)
    in x :+ y
  readOffAddr# addr# i# =
    \s0 -> case readOffAddr# addr# (2# *# i#) s0 of
       (# s1, x #) -> case readOffAddr# addr# (2# *# i# +# 1#) s1 of
         (# s2, y #) -> (# s2, x :+ y #)
  writeOffAddr# addr# i# (a :+ b) =
    \s0 -> case writeOffAddr# addr# (2# *# i#) a s0 of
       s1 -> case writeOffAddr# addr# (2# *# i# +# 1#) b s1 of
         s2 -> s2
  setOffAddr# = defaultSetOffAddr#
  {-# INLINE sizeOf# #-}
  {-# INLINE alignment# #-}
  {-# INLINE indexByteArray# #-}
  {-# INLINE readByteArray# #-}
  {-# INLINE writeByteArray# #-}
  {-# INLINE setByteArray# #-}
  {-# INLINE indexOffAddr# #-}
  {-# INLINE readOffAddr# #-}
  {-# INLINE writeOffAddr# #-}
  {-# INLINE setOffAddr# #-}
