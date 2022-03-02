module Dice where

import Control.Monad (replicateM)
import qualified Data.List as List
import Dist

die :: Int -> Dist Int
die n = uniform [1 .. n]

d :: Int -> Int -> Dist Int
n `d` k = sum <$> replicateM n (die k)

explode :: (Int -> Bool) -> Dist Int -> Dist [Int]
explode p r = do
  x <- r
  if p x
    then (x :) <$> explode p r
    else return [x]

limitedExplode :: Int -> (Int -> Bool) -> Dist Int -> Dist [Int]
limitedExplode rerolls p r = do
  x <- r
  if rerolls > 0 && p x
    then (x :) <$> limitedExplode (rerolls - 1) p r
    else return [x]

highest :: Ord a => Int -> [a] -> [a]
highest n = take n . reverse . List.sort

lowest :: Ord a => Int -> [a] -> [a]
lowest n = take n . List.sort

dropHighest :: Ord a => Int -> [a] -> [a]
dropHighest n = drop n . reverse . List.sort

dropLowest :: Ord a => Int -> [a] -> [a]
dropLowest n = drop n . List.sort
