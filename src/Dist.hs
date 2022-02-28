{-# LANGUAGE DeriveFunctor #-}

module Dist
  ( Probability,
    Dist,
    simplify,
    uniform,
    bernoulli,
    probability,
    conditional,
    sample,
  )
where

import Control.Monad (ap)
import Data.Bifunctor (first)
import Data.Function (on)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.PQueue.Prio.Max as MaxPQueue
import Data.Tuple (swap)
import System.Random (randomRIO)

type Probability = Rational

-- Invariant: non-increasing order of probability
newtype Dist a = Dist {unDist :: [(Probability, a)]}
  deriving (Functor, Show)

toDist :: [(Probability, a)] -> Dist a
toDist = Dist . List.sortBy (compare `on` negate . fst)

instance Applicative Dist where
  pure x = Dist [(1, x)]
  (<*>) = ap

instance Monad Dist where
  m >>= f = joinDist (fmap f m)

data JoinStep a
  = VisitRows (Dist (Dist a))
  | VisitOneRow Probability (Dist a)
  | EmitItem Probability a

joinDist :: Dist (Dist a) -> Dist a
joinDist dist =
  let queue = MaxPQueue.singleton 1 (VisitRows dist)
   in Dist (fromQueue queue)
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

simplify :: Ord a => Dist a -> Dist a
simplify r = Dist $ swap <$> Map.toList (Map.fromListWith (+) (swap <$> unDist r))

uniform :: [a] -> Dist a
uniform xs = Dist [(recip n, x) | x <- xs]
  where
    n = toRational (length xs)

bernoulli :: Probability -> Dist Bool
bernoulli p = toDist [(p, True), (1 - p, False)]

probability :: (a -> Bool) -> Dist a -> Probability
probability event = sum . fmap fst . filter (event . snd) . unDist

conditional :: (a -> Bool) -> Dist a -> Dist a
conditional event dist = Dist (map (first (/ p_event)) filtered)
  where
    filtered = filter (event . snd) (unDist dist)
    p_event = sum (map fst filtered)

sample :: Dist a -> IO a
sample dist = go (unDist dist) . toRational <$> randomRIO (0 :: Double, 1)
  where
    go [] _ = error "Dist.sample: empty distribution"
    go [(_, x)] _ = x
    go ((p, x) : xs) k
      | k < p = x
      | otherwise = go xs (k - p)
