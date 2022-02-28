module Dice where

import Control.Monad (replicateM)
import qualified Data.List as List
import Dist

die :: Int -> Dist Int
die n = uniform [1 .. n]

d :: Int -> Int -> Dist Int
n `d` k = sum <$> replicateM n (die k)

explode :: Int -> Dist Int -> Dist Int
explode threshold r = do
  x <- r
  if x >= threshold
    then (+ x) <$> explode threshold r
    else return x

limitedExplode :: Int -> Int -> Dist Int -> Dist Int
limitedExplode rerolls threshold r = do
  x <- r
  if rerolls > 0 && x >= threshold
    then (+ x) <$> limitedExplode (rerolls - 1) threshold r
    else return x

highest :: Ord a => Int -> [a] -> [a]
highest n = take n . reverse . List.sort

lowest :: Ord a => Int -> [a] -> [a]
lowest n = take n . List.sort

dropHighest :: Ord a => Int -> [a] -> [a]
dropHighest n = drop n . reverse . List.sort

dropLowest :: Ord a => Int -> [a] -> [a]
dropLowest n = drop n . List.sort
