{-# LANGUAGE BangPatterns #-}

{-# OPTIONS_GHC -O2 #-}

module Main (main) where

import System.IO
import System.Random

mkRandoms :: FilePath
          -> Int
          -> Int
          -> Int
          -> IO ()
mkRandoms !fp !l !u !i = do
  let go !ix !xs = if ix < u
        then do
          !x <- getStdRandom (randomR (ix, ix + i))
          go (ix + i) (x : xs)
        else return xs
  !ks <- go l []

  let gs :: String
      !gs = unlines $ reverse $ fmap show ks
  
  writeFile fp gs

main :: IO ()
main = do
  mkRandoms big lowerBound upperBound increment
  
  where
    big = "timestamps/ts.txt" 
    lowerBound = 0
    upperBound = 10 ^ 10
    increment  = 112
