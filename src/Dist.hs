{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}

module Dist
  ( -- * Types
    Probability,
    Dist,
    simplify,
    conditional,
    finiteConditional,

    -- * Common distributions
    categorical,
    uniform,
    geometric,
    bernoulli,
    binomial,
    negativeBinomial,
    hypergeometric,

    -- * Analysis
    probability,
    approxProbability,
    expectation,
    variance,
    stddev,
    possibilities,
    sample,
  )
where

import Control.Applicative (liftA2)
import Control.Monad (ap)
import Data.Bifunctor (first)
import Data.Bool (bool)
import qualified Data.Map as Map
import qualified Data.PQueue.Prio.Max as PQ
import Data.Ratio
import Data.Tuple (swap)
import System.Random (randomRIO)

-- | Probabilities are represented as rational numbers so we can ensure they add
-- to 1.
type Probability = Rational

-- | A probability distribution of values.
data Dist a
  = Certainly a
  | Choice Probability (Dist a) (Dist a)
  deriving (Functor, Show)

instance Applicative Dist where
  pure = Certainly
  (<*>) = ap

instance Monad Dist where
  Certainly x >>= f = f x
  Choice p a b >>= f = Choice p (a >>= f) (b >>= f)

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
simplify =
  categorical
    . fmap swap
    . Map.toList
    . Map.fromListWith (+)
    . fmap swap
    . possibilities

-- | Produces the conditional probability distribution, assuming some event.
-- This function works for all distributions, but always produces an infinite
-- distribution for non-trivial events.
conditional :: (a -> Bool) -> Dist a -> Dist a
conditional event dist = do
  x <- dist
  if event x
    then return x
    else conditional event dist

-- | Produces the conditional probability distribution, assuming some event.
--
-- This only works for finite distributions.  Infinite distributions (including
-- even distributions with finitely many outcomes, but infinitely many paths to
-- reach those outcomes) will hang.
finiteConditional :: (a -> Bool) -> Dist a -> Dist a
finiteConditional event dist = categorical (map (first (/ p_event)) filtered)
  where
    filtered = filter (event . snd) (possibilities dist)
    p_event = sum (map fst filtered)

-- | A distribution with a fixed probability for each outcome.  The
-- probabilities should add to 1, but this is not checked.
categorical :: [(Probability, a)] -> Dist a
categorical = go 1
  where
    go _ [] = error "Empty distribution is not allowed"
    go p ((q, x) : xs)
      | null xs || q >= p = Certainly x
      | otherwise = Choice (q / p) (Certainly x) (go (p - q) xs)

-- | A uniform distribution over a list of values.
uniform :: [a] -> Dist a
uniform xs = categorical $ (recip n,) <$> xs where n = toRational (length xs)

-- | Geometric distribution over a list of possibilities.
geometric :: Probability -> [a] -> Dist a
geometric _ [] = error "geometric: Empty distribution is not allowed"
geometric _ [x] = Certainly x
geometric p (x : xs) = Choice p (Certainly x) (geometric p xs)

-- | A Bernoulli distribution.  This gives True with probability @p@, and False
-- otherwise.
bernoulli :: Probability -> Dist Bool
bernoulli p = Choice p (Certainly True) (Certainly False)

-- | Computes nCk.  This is a building block for several well-known discrete
-- distributions.
choose :: Integer -> Integer -> Integer
n `choose` k
  | k > n `div` 2 = n `choose` (n - k)
  | otherwise = product [n - k + 1 .. n] `div` product [1 .. k]

-- | A binomial distribution.  This gives the distribution of number of
-- successes in @n@ trials with probability @p@ of success.
binomial :: Integer -> Probability -> Dist Integer
binomial n p =
  categorical
    [ (fromInteger (n `choose` k) * p ^ k * (1 - p) ^ (n - k), k)
      | k <- [0 .. n]
    ]

-- | Negative binomial distribution.  This gives the distribution of number of
-- failures before @r@ successes with probability @p@ of success.
negativeBinomial :: Integer -> Probability -> Dist Integer
negativeBinomial 0 _ = pure 0
negativeBinomial r p =
  categorical
    [ (fromInteger ((k + r - 1) `choose` (r - 1)) * p ^ r * (1 - p) ^ k, k)
      | k <- [0 ..]
    ]

-- | Hypergeometric distribution.  This gives the distribution of number of
-- successful draws out of @n@ attempts without replacement, when @k@
-- possibilities are successful.
hypergeometric :: Integer -> Integer -> Integer -> Dist Integer
hypergeometric n pop k =
  categorical
    [ ((k `choose` m) * ((pop - k) `choose` (n - m)) % (pop `choose` n), m)
      | m <- [lo .. hi]
    ]
  where
    lo = max 0 (n + k - pop)
    hi = min n k

-- | Computes the probability of an event, represented by a predicate on values.
--
-- This only works for finite distributions.  Infinite distributions (including
-- even distributions with finitely many outcomes, but infinitely many paths to
-- reach those outcomes) will hang.
probability :: (a -> Bool) -> Dist a -> Probability
probability event (Certainly x) = if event x then 1 else 0
probability event (Choice p a b) =
  p * probability event a + (1 - p) * probability event b

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
expectation (Certainly x) = x
expectation (Choice p a b) =
  fromRational p * expectation a + fromRational (1 - p) * expectation b

-- | Computes the variance of a finite distribution.
--
-- This only works for finite distributions.  Infinite distributions (including
-- even distributions with finitely many outcomes, but infinitely many paths to
-- reach those outcomes) will hang.
variance :: (Real a, Fractional a) => Dist a -> a
variance dist = expectation ((^ (2 :: Int)) . subtract mean <$> dist)
  where
    mean = realToFrac (expectation dist)

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
possibilities = step . PQ.singleton 1
  where
    step queue
      | PQ.null queue = []
      | otherwise = case PQ.deleteFindMax queue of
        ((p, dist), queue') -> handle p dist queue'

    handle p (Certainly x) queue = (p, x) : step queue
    handle p (Choice q a b) queue =
      step $ PQ.insert (p * q) a $ PQ.insert (p * (1 - q)) b queue

-- | Samples the probability distribution to produce a value.
sample :: Dist a -> IO a
sample (Certainly x) = return x
sample (Choice p a b) =
  bool (sample b) (sample a) . (< p) . toRational =<< randomRIO (0 :: Double, 1)
