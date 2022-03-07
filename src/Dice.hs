{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Useful functions for probability distributions relating to dice.
module Dice where

import Control.Monad (replicateM)
import qualified Data.List as List
import Data.Tuple (swap)
import Dist

coinFlip :: Fractional prob => Dist prob Bool
coinFlip = bernoulli 0.5

data Roll = Total Int | Dice [Int]

total :: Roll -> Int
total (Total n) = n
total (Dice ds) = sum ds

dice :: Roll -> [Int]
dice (Total _) = error "need individual dice for this calculation"
dice (Dice ds) = ds

instance Eq Roll where
  a == b = total a == total b

instance Ord Roll where
  compare a b = compare (total a) (total b)

instance Show Roll where
  show (Total n) = show n
  show (Dice ds) =
    show (sum ds) ++ " (" ++ List.intercalate ", " (show <$> ds) ++ ")"

instance Num Roll where
  a + b = Total (fromRoll a + fromRoll b)
  a - b = Total (fromRoll a - fromRoll b)
  a * b = Total (fromRoll a * fromRoll b)
  abs = Total . abs . fromRoll
  signum = Total . signum . fromRoll
  negate = Total . negate . fromRoll
  fromInteger = Total . fromInteger

instance Enum Roll where
  toEnum = Total . toEnum
  fromEnum = fromEnum . total

instance Integral Roll where
  quotRem a b = (Total q, Total r)
    where
      (q, r) = quotRem (fromRoll a) (fromRoll b)
  divMod a b = (Total q, Total r)
    where
      (q, r) = divMod (fromRoll a) (fromRoll b)
  toInteger = toInteger . total

instance Real Roll where
  toRational = toRational . total

instance Semigroup Roll where
  Dice a <> Dice b = Dice (a <> b)
  a <> b = Total (total a + total b)

class FromRoll a where fromRoll :: Roll -> a

instance FromRoll Roll where fromRoll = id

instance FromRoll Int where fromRoll = total

instance FromRoll [Int] where fromRoll = dice

die :: Fractional prob => Int -> Dist prob Roll
die n = Total <$> uniform [1 .. n]

d :: (Fractional prob) => Int -> Int -> Dist prob Roll
n `d` m = Dice <$> replicateM n (uniform [1 .. m])

forgetDice :: Dist prob Roll -> Dist prob Roll
forgetDice = fmap (Total . total)

rerollIf :: FromRoll a => (a -> Bool) -> Dist prob Roll -> Dist prob Roll
rerollIf p r = do
  x <- r
  if p (fromRoll x)
    then rerollIf p r
    else return x

limitedRerollIf ::
  FromRoll a => Int -> (a -> Bool) -> Dist prob Roll -> Dist prob Roll
limitedRerollIf rerolls p r = do
  x <- r
  if rerolls > 0 && p (fromRoll x)
    then limitedRerollIf (rerolls - 1) p r
    else return x

explodeIf :: FromRoll a => (a -> Bool) -> Dist prob Roll -> Dist prob Roll
explodeIf p r = do
  x <- r
  if p (fromRoll x)
    then (x <>) <$> explodeIf p r
    else return (Dice [total x])

limitedExplodeIf ::
  FromRoll a => Int -> (a -> Bool) -> Dist prob Roll -> Dist prob Roll
limitedExplodeIf rerolls p r = do
  x <- r
  if rerolls > 0 && p (fromRoll x)
    then (x <>) <$> limitedExplodeIf (rerolls - 1) p r
    else return (Dice [total x])

withDice :: ([Int] -> [Int]) -> Dist prob Roll -> Dist prob Roll
withDice f = fmap (Dice . f . dice)

highest :: Int -> Dist prob Roll -> Dist prob Roll
highest n = withDice (take n . reverse . List.sort)

lowest :: Int -> Dist prob Roll -> Dist prob Roll
lowest n = withDice (take n . List.sort)

dropHighest :: Int -> Dist prob Roll -> Dist prob Roll
dropHighest n = withDice (drop n . reverse . List.sort)

dropLowest :: Int -> Dist prob Roll -> Dist prob Roll
dropLowest n = withDice (drop n . List.sort)

only :: (Int -> Bool) -> Dist prob Roll -> Dist prob Roll
only p = withDice (filter p)

count :: Dist prob Roll -> Dist prob Roll
count = fmap (Total . length . dice)

dndStat :: Fractional prob => Dist prob Roll
dndStat = dropLowest 1 (4 `d` 6)

advantage :: Dist prob Roll -> Dist prob Roll
advantage r = List.maximum <$> replicateM 2 r

disadvantage :: Dist prob Roll -> Dist prob Roll
disadvantage r = List.minimum <$> replicateM 2 r

graph :: (Show prob, RealFrac prob) => Dist prob Roll -> String
graph roll = unlines (map showLine (List.sort $ fmap swap $ possibilities $ simplify (total <$> roll)))
  where
    showLine (r, p) =
      let n = round (70 * p)
       in replicate n '*' ++ replicate (70 - n) ' ' ++ ": "
            ++ (show r ++ " (" ++ show (100 * p) ++ "%)")
