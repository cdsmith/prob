module Main where

import Control.Monad (replicateM)
import Dice
import Dist
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)

-- Given a probability of success, this is the number of failures
-- before succeeding n times.
--
-- This demonstrates a bug: for some reason, negBinomial p 2 hangs
-- if p is less than the reciprocal of the golden ratio!
negBinomial :: Probability -> Int -> Dist Int
negBinomial _ 0 = return 0
negBinomial p r = do
  success <- bernoulli p
  if success
    then negBinomial p (r - 1)
    else (+ 1) <$> negBinomial p r

-- This is the D&D rule for rolling stats: roll 4d6 and drop the lowest.
dndStat :: Dist Int
dndStat = sum . dropLowest 1 <$> replicateM 3 (die 6)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn $ take 10000 $ show $ negBinomial 0.61803398875 2

roll2d6 :: Dist Int
roll2d6 = do
  a <- die 6
  b <- die 6
  return (a + b)

twiced6 :: Dist Int
twiced6 = do
  a <- die 6
  return (2 * a)

roll2d6Plus3 :: Dist Int
roll2d6Plus3 = do
  a <- die 6
  b <- die 6
  return (a + b + 3)

roll2d6Plusd4 :: Dist Int
roll2d6Plusd4 = do
  a <- die 6
  b <- die 6
  c <- die 4
  return (a + b + c)
