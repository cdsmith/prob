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
import Data.Bifunctor (bimap)
import qualified Data.Map as Map
import Data.Tuple (swap)
import System.Random (randomRIO)
import qualified Data.PQueue.Prio.Max as PQ

-- | Probabilities are represented as rational numbers so we can ensure they add
-- to 1.
type Probability = Rational

-- | A probability distribution of values.
data Dist a
  = Certainly a
  | Choice [(Probability, Dist a)]
  deriving (Functor, Show)

instance Applicative Dist where
  pure = Certainly
  (<*>) = ap

instance Monad Dist where
  Certainly x >>= f = f x
  Choice xs >>= f = Choice $ map (fmap (>>= f)) xs

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
simplify dist =
  Choice $ fmap Certainly . swap <$> Map.toList (Map.fromListWith (+) (swap <$> possibilities dist))

-- | A uniform distribution over a list of values.
uniform :: [a] -> Dist a
uniform xs = Choice [(recip n, Certainly x) | x <- xs]
  where
    n = toRational (length xs)

-- | A Bernoulli distribution.  This gives True with probability @p@, and False
-- otherwise.
bernoulli :: Probability -> Dist Bool
bernoulli p = Choice [(p, Certainly True), (1 - p, Certainly False)]

-- | Computes the probability of an event, represented by a predicate on values.
--
-- This only works for finite distributions.  Infinite distributions (including
-- even distributions with finitely many outcomes, but infinitely many paths to
-- reach those outcomes) will hang.
probability :: (a -> Bool) -> Dist a -> Probability
probability event = sum . fmap fst . filter (event . snd) . possibilities

-- | Like probability, but produces a lazy list of ever-improving ranges of
-- probabilities.  This can be used on infinite distributions, for which the
-- exact probability cannot be calculated.
approxProbability :: (a -> Bool) -> Dist a -> [(Probability, Probability)]
approxProbability event = go 0 1 . possibilities
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
expectation dist = sum [ fromRational p * x | (p, x) <- possibilities dist ]

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
possibilities :: Dist a -> [(Probability, a)]
possibilities dist = go (PQ.singleton 1 dist)
  where
    go queue
      | PQ.null queue = []
      | otherwise = case PQ.deleteFindMax queue of
        ((p, Certainly x), queue') -> (p, x) : go queue'
        ((p, Choice xs), queue') ->
          go (foldr (\(q, x) -> PQ.insert (p * q) x) queue' xs)

-- | Produces the conditional probability distribution, assuming some event.
--
-- This only works for finite distributions.  Infinite distributions (including
-- even distributions with finitely many outcomes, but infinitely many paths to
-- reach those outcomes) will hang.
finiteConditional :: (a -> Bool) -> Dist a -> Dist a
finiteConditional event dist = Choice (map (bimap (/ p_event) Certainly) filtered)
  where
    filtered = filter (event . snd) (possibilities dist)
    p_event = sum (map fst filtered)

-- | Produces the conditional probability distribution, assuming some event.
-- This function works for all distributions, but always produces an infinite
-- distribution for non-trivial events.
conditional :: (a -> Bool) -> Dist a -> Dist a
conditional event dist = do
  x <- dist
  if event x
    then return x
    else conditional event dist

-- | Samples the probability distribution to produce a value.
sample :: Dist a -> IO a
sample dist = go (possibilities dist) . toRational <$> randomRIO (0 :: Double, 1)
  where
    go [] _ = error "Dist.sample: empty distribution"
    go [(_, x)] _ = x
    go ((p, x) : xs) k
      | k < p = x
      | otherwise = go xs (k - p)
