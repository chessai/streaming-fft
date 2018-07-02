
module Util
  ( readFile
  ) where

import Prelude hiding (readFile)
import Streaming.Prelude (Of((:>)))
import Streaming.Internal (Stream(Return,Effect,Step))
import qualified Sreaming.Prelude as S

import qualified System.IO as IO
import Control.Monad.Trans.Resource

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

