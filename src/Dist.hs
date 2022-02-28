{-# LANGUAGE DeriveFunctor #-}

module Dist
  ( Probability,
    Dist,
    simplify,
    uniform,
    bernoulli,
    probability,
    possibilities,
    conditional,
    sample,
  )
where

import Control.Applicative (liftA2)
import Control.Monad (ap)
import Data.Bifunctor (first)
import Data.Function (on)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.PQueue.Prio.Max as MaxPQueue
import Data.Tuple (swap)
import System.Random (randomRIO)

-- | Probabilities are represented as rational numbers so we can ensure they add
-- to 1.
type Probability = Rational

-- | A probability distribution of values.
newtype Dist a = Dist
  { -- | Two invariants are maintained on the value:
    -- 1. Entries occur in non-increasing order of probability.
    -- 2. The sum of the probabilities is 1.
    --
    -- In general, this may contain duplicate values.  If the distribution is
    -- finite, then duplicate values can be removed by 'simplify'.
    unDist :: [(Probability, a)]
  }
  deriving (Functor, Show)

-- | A helper function to build a 'Dist' in the correct order.
toDist :: [(Probability, a)] -> Dist a
toDist = Dist . List.sortBy (compare `on` negate . fst)

instance Applicative Dist where
  pure x = Dist [(1, x)]
  (<*>) = ap

instance Monad Dist where
  m >>= f = joinDist (fmap f m)

-- | This is a helper type used for the task queue when joining distributions.
data JoinStep a
  = VisitRows (Dist (Dist a))
  | VisitOneRow Probability (Dist a)
  | EmitItem Probability a

-- The join function for the 'Dist' monad.  This is where the magic happens to
-- keep everything working for infinite distributions.  The idea is to lazily
-- produce entries in decreasing order by using a *finite* priority queue of
-- tasks.  The priority of a task is set to the maximum probability that might
-- be output as any (direct or indirect) result of that task running.
joinDist :: Dist (Dist a) -> Dist a
joinDist = Dist . fromQueue . MaxPQueue.singleton 1 . VisitRows
  where
    fromQueue queue
      | MaxPQueue.null queue = []
      | otherwise = case MaxPQueue.deleteFindMax queue of
        ((_, VisitRows (Dist [])), queue') -> fromQueue queue'
        ((_, VisitRows (Dist ((p, row) : rows))), queue') ->
          fromQueue $
            MaxPQueue.insert p (VisitOneRow p row) $
              MaxPQueue.insert p (VisitRows (Dist rows)) queue'
        ((_, VisitOneRow _ (Dist [])), queue') -> fromQueue queue'
        ((_, VisitOneRow p (Dist ((q, x) : xs))), queue') ->
          fromQueue $
            MaxPQueue.insert (p * q) (EmitItem (p * q) x) $
              MaxPQueue.insert (p * q) (VisitOneRow p (Dist xs)) queue'
        ((_, EmitItem p x), queue') -> (p, x) : fromQueue queue'

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

-- | Simplifies a finite distribution.  Note that this will hang for infinite
-- distributions.  Infinite distributions *include* cases where there are
-- finitely many outcomes, but infinitely many paths to reach those outcomes.
-- for example, the rule "Keep rolling a die until you get a result larger than
-- 2" is an infinite distribution.
simplify :: Ord a => Dist a -> Dist a
simplify r = toDist $ swap <$> Map.toList (Map.fromListWith (+) (swap <$> unDist r))

-- | A uniform distribution over a list of values.
uniform :: [a] -> Dist a
uniform xs = Dist [(recip n, x) | x <- xs]
  where
    n = toRational (length xs)

-- | A Bernoulli distribution.  This gives True with probability @p@, and False
-- otherwise.
bernoulli :: Probability -> Dist Bool
bernoulli p = toDist [(p, True), (1 - p, False)]

-- | Computes the probability of an event, represented by a predicate on values.
probability :: (a -> Bool) -> Dist a -> Probability
probability event = sum . fmap fst . filter (event . snd) . unDist

-- | Gives the list of all possibile values of a given probability distribution.
-- This will often contain duplicate values, which can be removed using 'nub',
-- 'Data.Set.fromList', etc.
possibilities :: Dist a -> [a]
possibilities = fmap snd . unDist

-- | Produces the conditional probability distribution, assuming some event.
-- The event is represented as a predicate on values.
conditional :: (a -> Bool) -> Dist a -> Dist a
conditional event dist = Dist (map (first (/ p_event)) filtered)
  where
    filtered = filter (event . snd) (unDist dist)
    p_event = sum (map fst filtered)

-- | Samples the probability distribution to produce a value.
sample :: Dist a -> IO a
sample dist = go (unDist dist) . toRational <$> randomRIO (0 :: Double, 1)
  where
    go [] _ = error "Dist.sample: empty distribution"
    go [(_, x)] _ = x
    go ((p, x) : xs) k
      | k < p = x
      | otherwise = go xs (k - p)
