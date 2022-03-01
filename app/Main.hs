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
dndStat = sum . dropLowest 1 <$> replicateM 4 (die 6)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn $ take 10000 $ show $ negBinomial 0.61803398874 2

roll2d6 :: Dist Int
roll2d6 = 2 `d` 6

rolld6Times2 :: Dist Int
rolld6Times2 = 2 * die 6

roll2d6Plus3 :: Dist Int
roll2d6Plus3 = 2 `d` 6 + 3

roll2d6Plusd4 :: Dist Int
roll2d6Plusd4 = 2 `d` 6 + die 4