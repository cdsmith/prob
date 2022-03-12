{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}

-- | This module defines the 'Dist' monad and its operations.
--
-- A @'Dist' a@ is a discrete probability distribution over values of type @a@.
--
-- You can define distributions in several ways:
--
-- * Choosing from the common distributions exported by this module, such as
--   a 'categorical', 'uniform', 'geometric', 'bernoulli', 'binomial',
--   'negativeBinomial', 'hypergemetric', or 'poisson' distribution.
-- * Operating on existing distributions using the 'Functor', 'Applicative', and
--   'Monad' instances, or by conditioning on events using 'conditional' or
--   'finiteConditional'.
--
-- Once you have a distribution, you can sample from it using 'sample', list its
-- outcomes and their probabilities using 'probabilities' or 'possibilities',
-- and compute various statistics using 'probability', 'approxProbability',
-- 'expectation', 'variance', 'stddev', 'entropy', 'relativeEntropy', or
-- 'mutualInformation'.
--
-- It's important to make a distinction between *finite* and *infinite*
-- distributions.  An infinite distribution is one whose list of 'possibilities'
-- is infinite.  Note that this *includes* distributions for which there are
-- only finitely many distinct outcomes, but still an infinite number of paths
-- to reach these outcomes.  Infinite distributions typically arise from
-- recursive expressions.  Certain functions only work on finite distributions,
-- and will hang or OOM if given an infinite distribution.
module Dist
  ( -- * Types
    Dist,
    possibilities,
    probabilities,
    simplify,
    sample,
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
    poisson,

    -- * Analysis
    probability,
    approxProbability,
    expectation,
    variance,
    stddev,
    entropy,
    relativeEntropy,
    mutualInformation,
  )
where

import Control.Applicative (liftA2)
import Control.Monad (ap)
import Data.Bifunctor (first)
import Data.Bool (bool)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Tuple (swap)
import System.Random (randomRIO)

-- | A probability distribution of values.
data Dist prob a
  = Certainly a
  | Choice prob (Dist prob a) (Dist prob a)
  deriving (Functor)

instance Applicative (Dist prob) where
  pure = Certainly
  (<*>) = ap

instance Monad (Dist prob) where
  Certainly x >>= f = f x
  Choice p a b >>= f = Choice p (a >>= f) (b >>= f)

instance Num a => Num (Dist prob a) where
  (+) = liftA2 (+)
  (-) = liftA2 (-)
  (*) = liftA2 (*)
  abs = fmap abs
  signum = fmap signum
  negate = fmap negate
  fromInteger = pure . fromInteger

instance Fractional a => Fractional (Dist prob a) where
  (/) = liftA2 (/)
  recip = fmap recip
  fromRational = pure . fromRational

-- | Gives the list of all possible values of a given probability distribution.
-- Possibilities are returned in decreasing order of probability.  However, the
-- list will often contain multiple entries for the same outcome, in which case
-- the true probability for that outcome is the sum of all entries.
--
-- In the finite case, multiple entries can be combined by using 'simplify' on
-- the 'Dist' first.
possibilities :: Num prob => Dist prob a -> [(prob, a)]
possibilities dist = go (Seq.singleton (1, dist))
  where
    go Seq.Empty = []
    go ((p, Certainly x) Seq.:<| queue') = (p, x) : go queue'
    go ((p, Choice q a b) Seq.:<| queue') =
      go (queue' Seq.:|> (p * q, a) Seq.:|> (p * (1 - q), b))

-- | Gives a map from outcomes to their probabilities in the given distribution.
--
-- This only works for finite distributions.  Infinite distributions (including
-- even distributions with finitely many outcomes, but infinitely many paths to
-- reach those outcomes) will hang.
probabilities :: (Num prob, Ord a) => Dist prob a -> Map a prob
probabilities = Map.fromListWith (+) . fmap swap . possibilities

-- | Simplifies a finite distribution.
--
-- This only works for finite distributions.  Infinite distributions (including
-- even distributions with finitely many outcomes, but infinitely many paths to
-- reach those outcomes) will hang.
simplify :: (Fractional prob, Ord a) => Dist prob a -> Dist prob a
simplify = categorical . fmap swap . Map.toList . probabilities

-- | Samples the probability distribution to produce a value.
sample :: (Real prob) => Dist prob a -> IO a
sample (Certainly x) = return x
sample (Choice p a b) =
  bool (sample b) (sample a) . (< realToFrac p) =<< randomRIO (0 :: Double, 1)

-- | Produces the conditional probability distribution, assuming some event.
-- This function works for all distributions, but always produces an infinite
-- distribution for non-trivial events.
conditional :: (a -> Bool) -> Dist prob a -> Dist prob a
conditional event dist = cdist
  where
    cdist = do
      x <- dist
      if event x
        then return x
        else cdist

-- | Produces the conditional probability distribution, assuming some event.
--
-- This only works for finite distributions.  Infinite distributions (including
-- even distributions with finitely many outcomes, but infinitely many paths to
-- reach those outcomes) will hang.
finiteConditional ::
  Fractional prob => (a -> Bool) -> Dist prob a -> Dist prob a
finiteConditional event dist = categorical (map (first (/ p_event)) filtered)
  where
    filtered = filter (event . snd) (possibilities dist)
    p_event = sum (map fst filtered)

-- | A distribution with a fixed probability for each outcome.  The
-- probabilities should add to 1, but this is not checked.
categorical :: Fractional prob => [(prob, a)] -> Dist prob a
categorical = go 1
  where
    go _ [] = error "Empty distribution is not allowed"
    go _ [(_, x)] = Certainly x
    go p ((q, x) : xs) = Choice (q / p) (Certainly x) (go (p - q) xs)

-- | A uniform distribution over a list of values.
uniform :: Fractional prob => [a] -> Dist prob a
uniform xs = categorical $ (recip n,) <$> xs where n = realToFrac (length xs)

-- | Geometric distribution over a list of possibilities.
geometric :: prob -> [a] -> Dist prob a
geometric _ [] = error "geometric: Empty distribution is not allowed"
geometric _ [x] = Certainly x
geometric p (x : xs) = Choice p (Certainly x) (geometric p xs)

-- | A Bernoulli distribution.  This gives True with probability @p@, and False
-- otherwise.
bernoulli :: prob -> Dist prob Bool
bernoulli p = Choice p (Certainly True) (Certainly False)

-- | Computes nCk.  This is a building block for several well-known discrete
-- distributions.
choose :: Integer -> Integer -> Integer
n `choose` k
  | k > n `div` 2 = n `choose` (n - k)
  | otherwise = product [n - k + 1 .. n] `div` product [1 .. k]

-- | A binomial distribution.  This gives the distribution of number of
-- successes in @n@ trials with probability @p@ of success.
binomial :: Fractional prob => Integer -> prob -> Dist prob Integer
binomial n p =
  categorical
    [ (fromInteger (n `choose` k) * p ^ k * (1 - p) ^ (n - k), k)
      | k <- [0 .. n]
    ]

-- | Negative binomial distribution.  This gives the distribution of number of
-- failures before @r@ successes with probability @p@ of success.
negativeBinomial :: Fractional prob => Integer -> prob -> Dist prob Integer
negativeBinomial 0 _ = pure 0
negativeBinomial r p =
  categorical
    [ (fromInteger ((k + r - 1) `choose` (r - 1)) * p ^ r * (1 - p) ^ k, k)
      | k <- [0 ..]
    ]

-- | Hypergeometric distribution.  This gives the distribution of number of
-- successful draws out of @n@ attempts without replacement, when @k@
-- possibilities are successful.
hypergeometric ::
  Fractional prob => Integer -> Integer -> Integer -> Dist prob Integer
hypergeometric n pop k =
  categorical
    [ ( fromInteger ((k `choose` m) * ((pop - k) `choose` (n - m)))
          / fromInteger (pop `choose` n),
        m
      )
      | m <- [lo .. hi]
    ]
  where
    lo = max 0 (n + k - pop)
    hi = min n k

-- | Poisson distribution.  Gives the number of independent events occurring in
-- a fixed time interval, if events are occurring at the given expected rate per
-- time interval.
poisson :: Floating prob => prob -> Dist prob Integer
poisson lambda =
  categorical
    [ (lambda ^ k * exp (-lambda) / fromInteger (product [1 .. k]), k)
      | k <- [0 ..]
    ]

-- | Computes the probability of an event, represented by a predicate on values.
--
-- This only works for finite distributions.  Infinite distributions (including
-- even distributions with finitely many outcomes, but infinitely many paths to
-- reach those outcomes) will hang.
probability :: Num prob => (a -> Bool) -> Dist prob a -> prob
probability event (Certainly x) = if event x then 1 else 0
probability event (Choice p a b) =
  p * probability event a + (1 - p) * probability event b

-- | Like probability, but produces a lazy list of ever-improving ranges of
-- probabilities.  This can be used on infinite distributions, for which the
-- exact probability cannot be calculated.
approxProbability :: Num prob => (a -> Bool) -> Dist prob a -> [(prob, prob)]
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
expectation :: Num a => Dist a a -> a
expectation (Certainly x) = x
expectation (Choice p a b) =
  p * expectation a + (1 - p) * expectation b

-- | Computes the variance of a finite distribution.
--
-- This only works for finite distributions.  Infinite distributions (including
-- even distributions with finitely many outcomes, but infinitely many paths to
-- reach those outcomes) will hang.
variance :: Num a => Dist a a -> a
variance dist = expectation ((^ (2 :: Int)) . subtract mean <$> dist)
  where
    mean = expectation dist

-- | Computes the standard deviation of a finite distribution.
--
-- This only works for finite distributions.  Infinite distributions (including
-- even distributions with finitely many outcomes, but infinitely many paths to
-- reach those outcomes) will hang.
stddev :: Floating a => Dist a a -> a
stddev dist = sqrt (variance dist)

-- | Computes the entropy of a distribution in bits.
--
-- This only works for finite distributions.  Infinite distributions (including
-- even distributions with finitely many outcomes, but infinitely many paths to
-- reach those outcomes) will hang.
entropy :: (Floating prob, Ord a) => Dist prob a -> prob
entropy dist =
  sum
    [ -p * logBase 2 p
      | p <- map fst (possibilities (simplify dist))
    ]

-- | Computes the relative entropy, also known as Kullback-Leibler divergence,
-- between two distributions in bits.
relativeEntropy ::
  (Eq prob, Floating prob, Ord a) => Dist prob a -> Dist prob a -> prob
relativeEntropy a b = sum (term <$> Set.toList vals)
  where
    prob_a = probabilities a
    prob_b = probabilities b
    vals = Map.keysSet prob_a `Set.union` Map.keysSet prob_b
    term x =
      let p = Map.findWithDefault 0 x prob_a
          q = Map.findWithDefault 0 x prob_b
       in if p == 0 then 0 else p * logBase 2 (p / q)

-- | Computes the mutual information between two random variables on the same
-- distribution, in bits.  A random variable is represented as a function from the type
-- of the underlying distribution to the type of values taken by the variable.
mutualInformation ::
  (Eq prob, Floating prob, Ord b, Ord c) =>
  Dist prob a ->
  (a -> b) ->
  (a -> c) ->
  prob
mutualInformation dist f g =
  sum (term <$> Map.keys f_probs <*> Map.keys g_probs)
  where
    joint_probs = probabilities ((\x -> (f x, g x)) <$> dist)
    f_probs = probabilities (f <$> dist)
    g_probs = probabilities (g <$> dist)
    term x y =
      let p_x = Map.findWithDefault 0 x f_probs
          p_y = Map.findWithDefault 0 y g_probs
          p_xy = Map.findWithDefault 0 (x, y) joint_probs
       in if p_xy == 0 then 0 else p_xy * logBase 2 (p_xy / (p_x * p_y))
