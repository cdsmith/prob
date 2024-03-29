{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}

-- | This module defines the 'Distribution' monad and its operations.
--
-- A @'Distribution' a@ is a discrete probability distribution over values of
-- type @a@.
--
-- You can define distributions in several ways:
--
-- * Choosing from the common distributions exported by this module, such as
--   a 'categorical', 'uniform', 'geometric', 'bernoulli', 'binomial',
--   'negativeBinomial', 'hypergeometric', or 'poisson' distribution.
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
--
-- For example, if you express the process of rolling a six-sided die, but
-- always rerolling if the result is one, then there are five distinct outcomes:
-- 2, 3, 4, 5, or 6.  Nevertheless, this is an infinite distribution, because
-- it's possible to roll any number of ones prior to the final result.
module Probability.Distribution
  ( -- * Types
    Distribution,
    Event,
    RandVar,
    EventView (..),

    -- * Basic operations
    possibilities,
    probabilities,
    simplify,
    sample,
    viewEvent,
    fromEventView,
    finitize,
    finitizeMaybe,
    conditional,
    finiteConditional,
    bayesian,
    finiteBayesian,

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
    probabilityBounds,
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
import Data.Bifunctor (Bifunctor (..))
import Data.Bool (bool)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Tuple (swap)
import System.Random (randomRIO)

-- | A probability distribution of values.
data Distribution prob a
  = Certainly a
  | Choice prob (Distribution prob a) (Distribution prob a)
  deriving (Functor)

instance Bifunctor Distribution where
  bimap _ g (Certainly a) = Certainly (g a)
  bimap f g (Choice p a b) = Choice (f p) (bimap f g a) (bimap f g b)

instance Applicative (Distribution prob) where
  pure = Certainly
  (<*>) = ap

instance Monad (Distribution prob) where
  Certainly x >>= f = f x
  Choice p a b >>= f = Choice p (a >>= f) (b >>= f)

instance Num a => Num (Distribution prob a) where
  (+) = liftA2 (+)
  (-) = liftA2 (-)
  (*) = liftA2 (*)
  abs = fmap abs
  signum = fmap signum
  negate = fmap negate
  fromInteger = pure . fromInteger

instance Fractional a => Fractional (Distribution prob a) where
  (/) = liftA2 (/)
  recip = fmap recip
  fromRational = pure . fromRational

-- | An event is a predicate on values from a sample space.
type Event s = s -> Bool

-- | A random variable is a function mapping each element of a sample space to
-- the corresponding value of the random variable.
type RandVar s a = s -> a

-- | Gives the list of all possible values of a given probability distribution.
-- The list will often contain multiple entries for the same outcome, in which
-- case the true probability for that outcome is the sum of probabilities of all
-- entries.
--
-- In the finite case, multiple entries can be combined by using 'simplify' on
-- the 'Distribution' first.
possibilities :: Num prob => Distribution prob a -> [(prob, a)]
possibilities dist = go (Seq.singleton (1, dist))
  where
    go Seq.Empty = []
    go ((p, Certainly x) Seq.:<| queue') = (p, x) : go queue'
    go ((p, Choice q a b) Seq.:<| queue') =
      go (queue' Seq.:|> (p * q, a) Seq.:|> (p * (1 - q), b))

-- | Truncates an infinite distribution to make it finite.  The epsilon
-- parameter is the amount of tail probability that you're willing to ignore
-- and assign to an arbitrary outcome.
finitize ::
  (Fractional prob, Ord prob) =>
  prob ->
  Distribution prob a ->
  Distribution prob a
finitize epsilon = categorical . go 1 . possibilities
  where
    go q ((p, x) : poss)
      | q - p < epsilon = [(q, x)]
      | otherwise = (p, x) : go (q - p) poss
    go _ [] = []

-- | Truncates an infinite distribution to make it finite.  This is equivalent
-- to the original distribution, except with some arbitrary set of outcomes with
-- probability less than epsilon replaced by Nothing.
finitizeMaybe ::
  (Fractional prob, Ord prob) =>
  prob ->
  Distribution prob a ->
  Distribution prob (Maybe a)
finitizeMaybe epsilon = categorical . go 1 . possibilities
  where
    go q ((p, x) : poss)
      | q - p < epsilon = [(q, Nothing)]
      | otherwise = (p, Just x) : go (q - p) poss
    go _ [] = []

-- | Gives a map from outcomes to their probabilities in the given distribution.
--
-- This only works for finite distributions.  Infinite distributions (including
-- even distributions with finitely many outcomes, but infinitely many paths to
-- reach those outcomes) will hang.
probabilities :: (Num prob, Ord a) => Distribution prob a -> Map a prob
probabilities = Map.fromListWith (+) . fmap swap . possibilities

-- | Simplifies a finite distribution.
--
-- This only works for finite distributions.  Infinite distributions (including
-- even distributions with finitely many outcomes, but infinitely many paths to
-- reach those outcomes) will hang.
simplify ::
  (Fractional prob, Ord a) => Distribution prob a -> Distribution prob a
simplify = categorical . fmap swap . Map.toList . probabilities

-- | Samples the probability distribution to produce a value.
sample :: Real prob => Distribution prob a -> IO a
sample (Certainly x) = return x
sample (Choice p a b) =
  bool (sample b) (sample a) . (< realToFrac p) =<< randomRIO (0 :: Double, 1)

-- | A view of a probability distribution from the point of view of a given
-- event.  The event either always happens, never happens, or happens sometimes
-- with some probability.  In the latter case, there are posterior distributions
-- for when the event does or does not happen.
data EventView prob s
  = Always (Distribution prob s)
  | Never (Distribution prob s)
  | Sometimes prob (Distribution prob s) (Distribution prob s)

-- | Gives a view on a probability distribution relative to some event.
--
-- The following are guaranteed.
-- 1. @'fromEventView' . 'viewEvent' ev = id@
-- 2. If @'viewEvent' ev dist = 'Always' dist'@, then @dist = dist'@ and
--    @'probability' ev dist = 1@.
-- 3. If @'viewEvent' ev dist = 'Never' dist'@, then @dist = dist'@ and
--    @'probability' ev dist = 0@.
-- 4. If @'viewEvent' ev dist = 'Sometimes' p a b@, then
--    @'probability' ev dist = p@ and:
--    * @dist = 'bernoulli' p >>= bool a b@
--    * @'probability' ev a = 1@
--    * @'probability' ev b = 0@
--
-- This only works for finite distributions.  Infinite distributions (including
-- even distributions with finitely many outcomes, but infinitely many paths to
-- reach those outcomes) will hang.
viewEvent ::
  Fractional prob =>
  Event s ->
  Distribution prob s ->
  EventView prob s
viewEvent event dist@(Certainly x)
  | event x = Always dist
  | otherwise = Never dist
viewEvent event dist@(Choice p aa bb) =
  case (viewEvent event aa, viewEvent event bb) of
    (Never _, Never _) -> Never dist
    (Always _, Always _) -> Always dist
    (Always a, Never b) -> Sometimes p a b
    (Never a, Always b) -> Sometimes (1 - p) b a
    (Sometimes q a1 a2, Never b) ->
      let (p', _, p2) = blend q 0 in Sometimes p' a1 (Choice p2 a2 b)
    (Sometimes q a1 a2, Always b) ->
      let (p', p1, _) = blend q 1 in Sometimes p' (Choice p1 a1 b) a2
    (Never a, Sometimes r b1 b2) ->
      let (p', _, p2) = blend 0 r in Sometimes p' b1 (Choice p2 a b2)
    (Always a, Sometimes r b1 b2) ->
      let (p', p1, _) = blend 1 r in Sometimes p' (Choice p1 a b1) b2
    (Sometimes q a1 a2, Sometimes r b1 b2) ->
      let (p', p1, p2) = blend q r
       in Sometimes p' (Choice p1 a1 b1) (Choice p2 a2 b2)
  where
    blend q r =
      let p' = p * q + (1 - p) * r
       in (p', p * q / p', p * (1 - q) / (1 - p'))

-- | Converts from 'EventView' back to a 'Distribution'.  The resulting
-- distribution is equivalent to the source distribution.
fromEventView :: EventView prob s -> Distribution prob s
fromEventView (Always dist) = dist
fromEventView (Never dist) = dist
fromEventView (Sometimes p a b) = Choice p a b

-- | Produces the conditional probability distribution, assuming some event.
-- This function works for all distributions, but always produces an infinite
-- distribution for non-trivial events.
conditional :: Event s -> Distribution prob s -> Distribution prob s
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
  Fractional prob => Event s -> Distribution prob s -> Distribution prob s
finiteConditional event dist = categorical (map (first (/ p_event)) filtered)
  where
    filtered = filter (event . snd) (possibilities dist)
    p_event = sum (map fst filtered)

-- | Updates a prior distribution of parameters for a model, based on an
-- observed event.  This implements Bayes' Law for distributions.
--
-- This function works for all distributions, but always produces an infinite
-- distribution for non-trivial events.
bayesian ::
  (param -> Distribution prob s) ->
  Event s ->
  Distribution prob param ->
  Distribution prob param
bayesian model event prior = posterior
  where
    posterior = do
      param <- prior
      x <- model param
      if event x then return param else posterior

-- | Updates a prior distribution of parameters for a model, based on an
-- observed event.  This implements Bayes' Law for distributions.
--
-- This only works for finite distributions.  Infinite distributions (including
-- even distributions with finitely many outcomes, but infinitely many paths to
-- reach those outcomes) will hang.
finiteBayesian ::
  Fractional prob =>
  (param -> Distribution prob s) ->
  Event s ->
  Distribution prob param ->
  Distribution prob param
finiteBayesian model event prior = case viewEvent (event . snd) withParam of
  Always dist -> fst <$> dist
  Never _ -> error "Posterior is undefined for an impossible event"
  Sometimes _ dist _ -> fst <$> dist
  where
    withParam = do
      param <- prior
      obs <- model param
      return (param, obs)

-- | A distribution with a fixed probability for each outcome.  The
-- probabilities should add to 1, but this is not checked.
categorical :: Fractional prob => [(prob, a)] -> Distribution prob a
categorical = go 1
  where
    go _ [] = error "Empty distribution is not allowed"
    go _ [(_, x)] = Certainly x
    go p ((q, x) : xs) = Choice (q / p) (Certainly x) (go (p - q) xs)

-- | A uniform distribution over a list of values.
uniform :: Fractional prob => [a] -> Distribution prob a
uniform xs = categorical $ (recip n,) <$> xs where n = realToFrac (length xs)

-- | Geometric distribution over a list of possibilities.
geometric :: prob -> [a] -> Distribution prob a
geometric _ [] = error "geometric: Empty distribution is not allowed"
geometric _ [x] = Certainly x
geometric p (x : xs) = Choice p (Certainly x) (geometric p xs)

-- | A Bernoulli distribution.  This gives True with probability @p@, and False
-- otherwise.
bernoulli :: prob -> Distribution prob Bool
bernoulli p = Choice p (Certainly True) (Certainly False)

-- | Computes nCk.  This is a building block for several well-known discrete
-- distributions.
choose :: Integral t => t -> t -> t
n `choose` k
  | k > n `div` 2 = n `choose` (n - k)
  | otherwise = product [n - k + 1 .. n] `div` product [1 .. k]

-- | A binomial distribution.  This gives the distribution of number of
-- successes in @n@ trials with probability @p@ of success.
binomial :: (Fractional prob, Integral n) => prob -> n -> Distribution prob n
binomial p n =
  categorical
    [ (fromIntegral (n `choose` k) * p ^ k * (1 - p) ^ (n - k), k)
      | k <- [0 .. n]
    ]

-- | Negative binomial distribution.  This gives the distribution of number of
-- failures before @r@ successes with probability @p@ of success.
negativeBinomial ::
  (Fractional prob, Integral n) => prob -> n -> Distribution prob n
negativeBinomial _ 0 = pure 0
negativeBinomial p r =
  categorical
    [ (fromIntegral ((k + r - 1) `choose` (r - 1)) * p ^ r * (1 - p) ^ k, k)
      | k <- [0 ..]
    ]

-- | Hypergeometric distribution.  This gives the distribution of number of
-- successful draws out of @n@ attempts without replacement, when @k@
-- possibilities are successful.
hypergeometric ::
  (Fractional prob, Integral n) => n -> n -> n -> Distribution prob n
hypergeometric pop k n =
  categorical
    [ ( fromIntegral ((k `choose` m) * ((pop - k) `choose` (n - m)))
          / fromIntegral (pop `choose` n),
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
poisson :: (Floating prob, Integral n) => prob -> Distribution prob n
poisson lambda =
  categorical
    [ (lambda ^ k * exp (-lambda) / fromIntegral (product [1 .. k]), k)
      | k <- [0 ..]
    ]

-- | Computes the probability of an event, represented by a predicate on values.
--
-- This only works for finite distributions.  Infinite distributions (including
-- even distributions with finitely many outcomes, but infinitely many paths to
-- reach those outcomes) will hang.
probability :: Num prob => Event s -> Distribution prob s -> prob
probability event (Certainly x) = if event x then 1 else 0
probability event (Choice p a b) =
  p * probability event a + (1 - p) * probability event b

-- | Like probability, but produces a lazy list of ever-improving bounds on the
-- probability.  This can be used on infinite distributions, for which the
-- exact probability cannot be calculated.
probabilityBounds ::
  Num prob => Event s -> Distribution prob s -> [(prob, prob)]
probabilityBounds event dist = go 0 1 (possibilities dist)
  where
    go p _ [] = [(p, p)]
    go p q ((q', x) : xs)
      | event x = (p, p + q) : go (p + q') (q - q') xs
      | otherwise = (p, p + q) : go p (q - q') xs

-- | Like probability, but produces a value that differs from the true
-- probability by at most epsilon. This can be used on infinite distributions,
-- for which the exact probability cannot be calculated.
approxProbability ::
  (Ord prob, Fractional prob) =>
  prob ->
  Event s ->
  Distribution prob s ->
  prob
approxProbability epsilon event dist =
  (/ 2) . uncurry (+) . head . dropWhile ((> epsilon) . abs . uncurry (-)) $
    probabilityBounds event dist

-- | Computes the expected value of a finite distribution.
--
-- This only works for finite distributions.  Infinite distributions (including
-- even distributions with finitely many outcomes, but infinitely many paths to
-- reach those outcomes) will hang.
expectation :: Num a => Distribution a a -> a
expectation (Certainly x) = x
expectation (Choice p a b) =
  p * expectation a + (1 - p) * expectation b

-- | Computes the variance of a finite distribution.
--
-- This only works for finite distributions.  Infinite distributions (including
-- even distributions with finitely many outcomes, but infinitely many paths to
-- reach those outcomes) will hang.
variance :: Num a => Distribution a a -> a
variance dist = expectation ((^ (2 :: Int)) . subtract mean <$> dist)
  where
    mean = expectation dist

-- | Computes the standard deviation of a finite distribution.
--
-- This only works for finite distributions.  Infinite distributions (including
-- even distributions with finitely many outcomes, but infinitely many paths to
-- reach those outcomes) will hang.
stddev :: Floating a => Distribution a a -> a
stddev dist = sqrt (variance dist)

-- | Computes the entropy of a distribution in bits.
--
-- This only works for finite distributions.  Infinite distributions (including
-- even distributions with finitely many outcomes, but infinitely many paths to
-- reach those outcomes) will hang.
entropy :: (Floating prob, Ord a) => Distribution prob a -> prob
entropy dist =
  sum
    [ -p * logBase 2 p
      | p <- map fst (possibilities (simplify dist))
    ]

-- | Computes the relative entropy, also known as Kullback-Leibler divergence,
-- between two distributions in bits.
--
-- This only works for finite distributions.  Infinite distributions (including
-- even distributions with finitely many outcomes, but infinitely many paths to
-- reach those outcomes) will hang.
relativeEntropy ::
  (Eq prob, Floating prob, Ord a) =>
  Distribution prob a ->
  Distribution prob a ->
  prob
relativeEntropy post prior = sum (term <$> Set.toList vals)
  where
    prob_post = probabilities post
    prob_prior = probabilities prior
    vals = Map.keysSet prob_post `Set.union` Map.keysSet prob_prior
    term x =
      let p = Map.findWithDefault 0 x prob_post
          q = Map.findWithDefault 0 x prob_prior
       in if p == 0 then 0 else p * logBase 2 (p / q)

-- | Computes the mutual information between two random variables, in bits.  The
-- given distribution is taken as a definition of a probability space, and the
-- random variables are represented as functions from the sample space to values
-- taken by the random variable.
--
-- This only works for finite distributions.  Infinite distributions (including
-- even distributions with finitely many outcomes, but infinitely many paths to
-- reach those outcomes) will hang.
mutualInformation ::
  (Eq prob, Floating prob, Ord a, Ord b) =>
  RandVar s a ->
  RandVar s b ->
  Distribution prob s ->
  prob
mutualInformation f g dist =
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
