{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples       #-}

module Streaming.FFT.Internal.Streaming
 ( Stream, Of(..)
 , chunky
 , toSliding
 , readFile 
 ) where

import Prelude hiding (sum, readFile)
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

import Streaming (Stream, chunksOf, lift)
import Streaming.Prelude (mapped, sum)

import Streaming.FFT.Types
import Streaming.Internal (Stream(Return,Effect,Step))
import Streaming.Prelude (Of((:>)))
import qualified Streaming.Prelude as SMP
import qualified Streaming.Prelude as S

import qualified System.IO as IO
import Control.Monad.Trans.Resource

slidingWindow :: forall a m b. (Prim a, PrimMonad m)
  => Int
  -> Stream (Of a) m b
  -> Stream (Of (MutablePrimArray (PrimState m) a)) m b
{-# INLINABLE slidingWindow #-}
slidingWindow !n' stream = Effect $ do
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

-- | Convert a stream into a stream of sliding windows
toSliding :: (Prim a, PrimMonad m)
  => Signal a
  -> Stream (Of a) m b
  -> Stream (Of (MutablePrimArray (PrimState m) a)) m b
toSliding (Signal sig) stream = slidingWindow sig stream
{-# INLINABLE toSliding #-}

-- | Bin a stream, then turn it into a sliding window of the signal size
chunky :: (Prim a, Num a, PrimMonad m)
  => Bin a
  -> Signal a
  -> Stream (Of a) m b
  -> Stream (Of (MutablePrimArray (PrimState m) a)) m b
chunky (Bin b) s strm = toSliding s $ mapped sum $ chunksOf b strm
{-# INLINABLE chunky #-}

--bracketStream ::

readFile :: (MonadResource m)
  => FilePath
  -> Stream (Of String) m ()
readFile fp = bracketStream (IO.openFile fp IO.ReadMode) (IO.hClose) S.fromHandle

bracketStream :: (Functor f, MonadResource m)
  => IO a
  -> (a -> IO ())
  -> (a -> Stream f m b)
  -> Stream f m b
bracketStream alloc free inside = do
  (key, seed) <- lift (allocate alloc free)
  clean key (inside seed)
  where
    clean key = loop where
      loop str = case str of
        Return r -> Effect (release key >> return (Return r))
        Effect m -> Effect (fmap loop m)
        Step f   -> Step (fmap loop f)


