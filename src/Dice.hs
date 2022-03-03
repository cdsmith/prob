{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Useful functions for probability distributions relating to dice.
module Dice where

import Control.Applicative (liftA2)
import Control.Monad (replicateM)
import qualified Data.List as List
import Dist (Dist, bernoulli, uniform)

-- It's common to do math with dice, as in "3d6 + 5".  To allow this, we define
-- a Num instance for distributions.  This is an orphan instance.
instance (a ~ Int) => Num (Dist prob a) where
  (+) = liftA2 (+)
  (-) = liftA2 (-)
  (*) = liftA2 (*)
  abs = fmap abs
  signum = fmap signum
  fromInteger = pure . fromInteger
  negate = fmap negate

coin :: Fractional prob => Dist prob Bool
coin = bernoulli 0.5

d :: (Ord prob, Fractional prob) => Int -> Int -> Dist prob [Int]
n `d` m = replicateM n (uniform [1 .. m])

rerollOn :: (a -> Bool) -> Dist prob a -> Dist prob a
rerollOn p r = do
  x <- r
  if p x
    then rerollOn p r
    else return x

limitedRerollOn :: Int -> (a -> Bool) -> Dist prob a -> Dist prob a
limitedRerollOn rerolls p r = do
  x <- r
  if rerolls > 0 && p x
    then limitedRerollOn (rerolls - 1) p r
    else return x

explodeOn :: (a -> Bool) -> Dist prob a -> Dist prob [a]
explodeOn p r = do
  x <- r
  if p x
    then (x :) <$> explodeOn p r
    else return [x]

limitedExplodeOn :: Int -> (a -> Bool) -> Dist prob a -> Dist prob [a]
limitedExplodeOn rerolls p r = do
  x <- r
  if rerolls > 0 && p x
    then (x :) <$> limitedExplodeOn (rerolls - 1) p r
    else return [x]

highest :: Int -> Dist prob [Int] -> Dist prob [Int]
highest n = fmap (take n . reverse . List.sort)

lowest :: Int -> Dist prob [Int] -> Dist prob [Int]
lowest n = fmap (take n . List.sort)

dropHighest :: Int -> Dist prob [Int] -> Dist prob [Int]
dropHighest n = fmap (drop n . reverse . List.sort)

dropLowest :: Int -> Dist prob [Int] -> Dist prob [Int]
dropLowest n = fmap (drop n . List.sort)

only :: (Int -> Bool) -> Dist prob [Int] -> Dist prob [Int]
only p = fmap (filter p)

count :: Dist prob [a] -> Dist prob Int
count = fmap length

total :: Dist prob [Int] -> Dist prob Int
total = fmap sum

dndStat :: (Ord prob, Fractional prob) => Dist prob Int
dndStat = total (dropLowest 1 (4 `d` 6))

advantage :: Dist prob Int -> Dist prob Int
advantage r = List.maximum <$> replicateM 2 r

disadvantage :: Dist prob Int -> Dist prob Int
disadvantage r = List.minimum <$> replicateM 2 r
