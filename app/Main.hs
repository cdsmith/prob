{-# OPTIONS_GHC -Wno-type-defaults #-}

module Main where

import Data.Bifunctor (bimap)
import Dice
import Dist
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)

-- This is the D&D rule for rolling stats: roll 4d6 and drop the lowest.

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  print $
    map (bimap fromRational fromRational) $
      take 1000 $
        approxProbability (> 5) $ finiteConditional (> 2) (total (1 `d` 6))
