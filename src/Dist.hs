{-# LANGUAGE DeriveFunctor #-}

module Dist
  ( Probability,
    Dist,
    simplify,
    uniform,
    bernoulli,
    probability,
    approxProbability,
    expectation,
    variance,
    stddev,
    possibilities,
    finiteConditional,
    conditional,
    sample,
  )
where

import Control.Applicative (liftA2)
import Control.Monad (ap)
import Data.Bifunctor (first)
import qualified Data.Map as Map
import Data.Tuple (swap)
import System.Random (randomRIO)

-- | Probabilities are represented as rational numbers so we can ensure they add
-- to 1.
type Probability = Rational

-- | A probability distribution of values.
newtype Dist a = Dist {unDist :: [(Probability, a)]}
  deriving (Functor, Show)

instance Applicative Dist where
  pure x = Dist [(1, x)]
  (<*>) = ap

instance Monad Dist where
  m >>= f = joinDist (fmap f m)

-- The join function for the 'Dist' monad.  This is a little tricky because it
-- needs to work properly for infinite distributions.
joinDist :: Dist (Dist a) -> Dist a
joinDist (Dist dist) =
  Dist (flatten [[(p * q, x) | (q, x) <- dist'] | (p, Dist dist') <- dist])

flatten :: [[a]] -> [a]
flatten [] = []
flatten (x : xs) = interleave x (flatten xs)
  where
    interleave [] ys = ys
    interleave (z : zs) ys = z : interleave ys zs

instance Num a => Num (Dist a) where
  (+) = liftA2 (+)
  (-) = liftA2 (-)
  (*) = liftA2 (*)
  abs = fmap abs
  signum = fmap signum
  fromInteger = pure . fromInteger
  negate = fmap negate

instance Fractional a => Fractional (Dist a) where
  recip = fmap recip
  fromRational = pure . fromRational

-- | Simplifies a finite distribution.
--
-- This only works for finite distributions.  Infinite distributions (including
-- even distributions with finitely many outcomes, but infinitely many paths to
-- reach those outcomes) will hang.
simplify :: Ord a => Dist a -> Dist a
simplify (Dist dist) =
  Dist $ swap <$> Map.toList (Map.fromListWith (+) (swap <$> dist))

-- | A uniform distribution over a list of values.
uniform :: [a] -> Dist a
uniform xs = Dist [(recip n, x) | x <- xs]
  where
    n = toRational (length xs)

-- | A Bernoulli distribution.  This gives True with probability @p@, and False
-- otherwise.
bernoulli :: Probability -> Dist Bool
bernoulli p = Dist [(p, True), (1 - p, False)]

-- | Computes the probability of an event, represented by a predicate on values.
--
-- This only works for finite distributions.  Infinite distributions (including
-- even distributions with finitely many outcomes, but infinitely many paths to
-- reach those outcomes) will hang.
probability :: (a -> Bool) -> Dist a -> Probability
probability event = sum . fmap fst . filter (event . snd) . unDist

-- | Like probability, but produces a lazy list of ever-improving ranges of
-- probabilities.  This can be used on infinite distributions, for which the
-- exact probability cannot be calculated.
approxProbability :: (a -> Bool) -> Dist a -> [(Probability, Probability)]
approxProbability event = go 0 1 . unDist
  where
    go p _ [] = [(p, p)]
    go p q ((q', x) : xs)
      | event x = (p, p + q) : go (p + q') (q - q') xs
      | otherwise = (p, p + q) : go p (q - q') xs

-- | Computes the expected value of a finite distribution.
--
-- This only works for finite distributions.  Infinite distributions (including
-- even distributions with finitely many outcomes, but infinitely many paths to
-- reach those outcomes) will hang.
expectation :: Fractional a => Dist a -> a
expectation (Dist dist) = sum [ fromRational p * x | (p, x) <- dist ]

-- | Computes the variance of a finite distribution.
--
-- This only works for finite distributions.  Infinite distributions (including
-- even distributions with finitely many outcomes, but infinitely many paths to
-- reach those outcomes) will hang.
variance :: (Real a, Fractional a) => Dist a -> a
variance dist = expectation ((^ (2 :: Int)) . subtract mean <$> dist)
  where mean = realToFrac (expectation dist)

-- | Computes the standard deviation of a finite distribution.
--
-- This only works for finite distributions.  Infinite distributions (including
-- even distributions with finitely many outcomes, but infinitely many paths to
-- reach those outcomes) will hang.
stddev :: (Real a, Floating a) => Dist a -> a
stddev dist = sqrt (variance dist)

-- | Gives the list of all possibile values of a given probability distribution.
-- This will often contain duplicate values, which can be removed using 'nub',
-- 'Data.Set.fromList', etc.
possibilities :: Dist a -> [a]
possibilities = fmap snd . unDist

-- | Produces the conditional probability distribution, assuming some event.
--
-- This only works for finite distributions.  Infinite distributions (including
-- even distributions with finitely many outcomes, but infinitely many paths to
-- reach those outcomes) will hang.
finiteConditional :: (a -> Bool) -> Dist a -> Dist a
finiteConditional event dist = Dist (map (first (/ p_event)) filtered)
  where
    filtered = filter (event . snd) (unDist dist)
    p_event = sum (map fst filtered)

-- | Produces the conditional probability distribution, assuming some event.
-- This function works for all distributions, but always produces an infinite
-- distribution for non-trivial events.
conditional :: (a -> Bool) -> Dist a -> Dist a
conditional event (Dist dist) = Dist table
  where
    table = flatten (matches : replacements)
    matches = filter (event . snd) dist
    unmatches = filter (not . event . snd) dist
    replacements = [ map (first (* p)) table | (p, _) <- unmatches ]

-- | Samples the probability distribution to produce a value.
sample :: Dist a -> IO a
sample dist = go (unDist dist) . toRational <$> randomRIO (0 :: Double, 1)
  where
    go [] _ = error "Dist.sample: empty distribution"
    go [(_, x)] _ = x
    go ((p, x) : xs) k
      | k < p = x
      | otherwise = go xs (k - p)
