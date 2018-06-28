{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wall #-}

module Main (main) where

import Control.Monad.Trans.Resource (MonadResource, runResourceT)
import Data.Complex (Complex(..))
import Prelude hiding (readFile)
import Streaming.FFT
import Streaming.FFT.Internal.Streaming (readFile)
import Streaming.FFT.Types
import Text.Read (readMaybe)
import qualified Streaming as S
import qualified Streaming.Prelude as S

stringToNum :: (Num a, Read a) => String -> a
stringToNum s = maybe 0 id (readMaybe s)
{-# INLINE stringToNum #-}

streamFP :: FilePath
streamFP = "timestamps/ts.txt"

ok :: Int; ok = 10000;
sigSize, binSize :: Int
-- | Each signal contains 'sigSize' number of bins
sigSize = 50
-- | Each bin contains 'binSize' number of 1d datapoints
binSize = ok `div` sigSize

thresholdSize :: Complex Double
thresholdSize = 15000 :+ 0

bin :: Bin Double; bin = Bin binSize
sig :: Signal Double; sig = Signal sigSize
thr :: Threshold Double; thr = Threshold thresholdSize

main :: IO ()
main = do
  let k :: MonadResource m => S.Stream (S.Of String) m ()
      k = readFile streamFP 
      timestamps :: MonadResource m => S.Stream (S.Of Double) m () 
      timestamps = S.map stringToNum k
  
  runResourceT $ S.print $ streamFFTDebug thr bin sig timestamps
  runResourceT $ S.print $ streamFFT thr bin sig timestamps
