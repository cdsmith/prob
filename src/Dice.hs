{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Useful functions for probability distributions relating to dice.
module Dice where

import Control.Applicative (liftA2)
import Control.Monad (replicateM)
import qualified Data.List as List
import Dist (Dist, bernoulli, uniform)

newtype Roll a = Roll {unRoll :: Dist Double a}
  deriving (Functor, Applicative, Monad)

-- | It's common to do math with dice, as in "3d6 + 5".  To allow this, we
-- define a Num instance for Roll.
instance Num a => Num (Roll a) where
  Roll a + Roll b = Roll (liftA2 (+) a b)
  Roll a - Roll b = Roll (liftA2 (-) a b)
  Roll a * Roll b = Roll (liftA2 (*) a b)
  abs = Roll . fmap abs . unRoll
  signum = Roll . fmap signum . unRoll
  negate = Roll . fmap negate . unRoll
  fromInteger = Roll . pure . fromInteger

instance Fractional a => Fractional (Roll a) where
  Roll a / Roll b = Roll (liftA2 (/) a b)
  recip = Roll . fmap recip . unRoll
  fromRational = Roll . pure . fromRational

coin :: Roll Bool
coin = Roll (bernoulli 0.5)

d :: Int -> Int -> Roll [Int]
n `d` m = Roll (replicateM n (uniform [1 .. m]))

rerollOn :: (a -> Bool) -> Roll a -> Roll a
rerollOn p r = do
  x <- r
  if p x
    then rerollOn p r
    else return x

limitedRerollOn :: Int -> (a -> Bool) -> Roll a -> Roll a
limitedRerollOn rerolls p r = do
  x <- r
  if rerolls > 0 && p x
    then limitedRerollOn (rerolls - 1) p r
    else return x

explodeOn :: (a -> Bool) -> Roll a -> Roll [a]
explodeOn p r = do
  x <- r
  if p x
    then (x :) <$> explodeOn p r
    else return [x]

limitedExplodeOn :: Int -> (a -> Bool) -> Roll a -> Roll [a]
limitedExplodeOn rerolls p r = do
  x <- r
  if rerolls > 0 && p x
    then (x :) <$> limitedExplodeOn (rerolls - 1) p r
    else return [x]

highest :: Int -> Roll [Int] -> Roll [Int]
highest n = fmap (take n . reverse . List.sort)

lowest :: Int -> Roll [Int] -> Roll [Int]
lowest n = fmap (take n . List.sort)

dropHighest :: Int -> Roll [Int] -> Roll [Int]
dropHighest n = fmap (drop n . reverse . List.sort)

dropLowest :: Int -> Roll [Int] -> Roll [Int]
dropLowest n = fmap (drop n . List.sort)

only :: (Int -> Bool) -> Roll [Int] -> Roll [Int]
only p = fmap (filter p)

count :: Roll [a] -> Roll Int
count = fmap length

total :: Roll [Int] -> Roll Int
total = fmap sum

dndStat :: Roll Int
dndStat = total (dropLowest 1 (4 `d` 6))

advantage :: Roll Int -> Roll Int
advantage r = List.maximum <$> replicateM 2 r

disadvantage :: Roll Int -> Roll Int
disadvantage r = List.minimum <$> replicateM 2 r
