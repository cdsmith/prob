{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad (replicateM, replicateM_)
import Data.List (sort)
import Probability.Distribution
import Test.Hspec

-- | A probability distribution with Rational probabilities.  This is convenient
-- in testing so that we can check for exact values.
type ExactDist a = Distribution Rational a

-- | A probability distribution with Double probabilities and values.  This is
-- inexact, but allows the maximum flexibility.
type DoubleDist = Distribution Double Double

epsilon :: Fractional a => a
epsilon = 1e-7

approxEq :: (Ord a, Fractional a) => a -> a -> Bool
approxEq x y = abs (x - y) < 1e-5

shouldApprox :: (Show a, Ord a, Fractional a) => a -> a -> Expectation
x `shouldApprox` y = x `shouldSatisfy` approxEq y

main :: IO ()
main = hspec $ do
  describe "standard distributions" $ do
    describe "categorical" $ do
      it "assigns the right probabilities" $ do
        let dist =
              categorical [(1 / 2, 1), (1 / 3, 2), (1 / 6, 3)] :: ExactDist Int
        probability (== 1) dist `shouldBe` 1 / 2
        probability (== 2) dist `shouldBe` 1 / 3
        probability (== 3) dist `shouldBe` 1 / 6

    describe "uniform" $ do
      it "assigns the right probabilities" $ do
        let dist = uniform [1, 2, 3] :: ExactDist Int
        probability (== 1) dist `shouldBe` 1 / 3
        probability (== 2) dist `shouldBe` 1 / 3
        probability (== 3) dist `shouldBe` 1 / 3

    describe "geometric" $ do
      it "assigns the right probabilities" $ do
        let dist = geometric (1 / 3) [1 .. 10] :: ExactDist Int
        probability (== 1) dist `shouldBe` 1 / 3
        probability (== 2) dist `shouldBe` 2 / 9
        probability (== 3) dist `shouldBe` 4 / 27

    describe "bernoulli" $ do
      it "assigns the right probabilities" $ do
        let dist = bernoulli (1 / 3) :: ExactDist Bool
        probability (== True) dist `shouldBe` 1 / 3
        probability (== False) dist `shouldBe` 2 / 3

    describe "binomial" $ do
      it "assigns the right probabilities" $ do
        let dist = binomial (1 / 3) 3 :: ExactDist Int
        probability (== 0) dist `shouldBe` 08 / 27
        probability (== 1) dist `shouldBe` 12 / 27
        probability (== 2) dist `shouldBe` 06 / 27
        probability (== 3) dist `shouldBe` 01 / 27

    describe "negativeBinomial" $ do
      it "assigns the right probabilities" $ do
        let dist = negativeBinomial (1 / 3) 3 :: ExactDist Int
        approxProbability epsilon (== 0) dist `shouldApprox` (1 / 27)
        approxProbability epsilon (== 1) dist `shouldApprox` (2 / 27)
        approxProbability epsilon (== 2) dist `shouldApprox` (8 / 81)
        approxProbability epsilon (== 3) dist `shouldApprox` (80 / 729)

    describe "hypergeometric" $ do
      it "assigns the right probabilities" $ do
        let dist = hypergeometric 10 5 3 :: ExactDist Int
        probability (== 0) dist `shouldBe` 6 / 72
        probability (== 1) dist `shouldBe` 30 / 72
        probability (== 2) dist `shouldBe` 30 / 72
        probability (== 3) dist `shouldBe` 6 / 72

    describe "poisson" $ do
      it "assigns the right probabilities" $ do
        let dist = poisson 3 :: Distribution Double Int
        approxProbability epsilon (== 0) dist `shouldApprox` (1 / exp 3)
        approxProbability epsilon (== 1) dist `shouldApprox` (3 / exp 3)
        approxProbability epsilon (== 2) dist `shouldApprox` (4.5 / exp 3)
        approxProbability epsilon (== 3) dist `shouldApprox` (4.5 / exp 3)

  describe "conditionals and splits" $ do
    it "splits a distribution" $ do
      let dist = uniform [1 .. 10] :: ExactDist Int
      case viewEvent (> 7) dist of
        Sometimes p a b -> do
          p `shouldBe` 3 / 10
          probability even a `shouldBe` 2 / 3
          probability even b `shouldBe` 3 / 7
        _ -> error "viewEvent gave the wrong answer"

  describe "Dist" $ do
    it "can describe a stochastic process" $ do
      let dist :: ExactDist [Int] = do
            -- Roll d4, then roll that number of d6 and add the d6's together.
            n <- uniform [1 .. 4]
            replicateM n (uniform [1 .. 6])
      replicateM_ 1000 $ do
        xs <- sample dist
        length xs `shouldSatisfy` (>= 1)
        length xs `shouldSatisfy` (<= 4)
        xs `shouldSatisfy` all (>= 1)
        xs `shouldSatisfy` all (<= 6)

    it "can describe an unbounded stochastic process" $ do
      -- Reroll all 1s.  This is a left-recursive infinite process, and naive
      -- implementations tend to hang when trying to analyze it.
      let dist :: ExactDist Int = do
            n <- uniform [1 .. 6]
            if n == 1 then dist else return n

      approxProbability epsilon (== 1) dist `shouldApprox` 0
      approxProbability epsilon (== 2) dist `shouldApprox` 0.2
      approxProbability epsilon (== 3) dist `shouldApprox` 0.2
      approxProbability epsilon (== 4) dist `shouldApprox` 0.2
      approxProbability epsilon (== 5) dist `shouldApprox` 0.2
      approxProbability epsilon (== 6) dist `shouldApprox` 0.2

  describe "stats" $ do
    it "computes the expected value of a distribution" $ do
      expectation (uniform [1 .. 5] :: ExactDist Rational) `shouldBe` 3

      let dist = finitize epsilon (geometric 0.5 [1 ..]) :: DoubleDist
      expectation dist `shouldApprox` 2

    it "computes the variance of a distribution" $ do
      let dist = finitize epsilon (fromInteger <$> poisson 4) :: DoubleDist
      expectation dist `shouldApprox` 4
      variance dist `shouldApprox` 4
      stddev dist `shouldApprox` 2

  describe "info theory" $ do
    it "example" $ do
      -- Our fundamental unit of information theory will be the bit, and the
      -- entropy* of a probability distribution tells us how many bits of
      -- information you would learn by knowing a value chosen from that
      -- distribution.
      --
      -- A choice between two options can be represented with one bit.
      entropy (uniform [0, 1] :: Distribution Double Int) `shouldApprox` 1

      -- On the other hand, choosing between 32 options requires 5 bits.
      entropy (uniform [1 .. 32] :: Distribution Double Int) `shouldApprox` 5

      -- The picture gets messier if the distribution is not uniform.  If
      -- you flip a biased coin that almost always comes up heads, then you
      -- nearly know the outcome before the coin flip even happens.  The entropy
      -- of this distribution is low.  You can also think of entropy as a
      -- measure of uncertainty.
      entropy (bernoulli 0.999 :: Distribution Double Bool)
        `shouldApprox` 0.011407757737461145

      -- We can explore more about information theory with a more advanced
      -- example.  We'll use the stochastic process from earlier, where we roll
      -- a d4, then roll that number of d6.
      let dist :: Distribution Double [Int] = do
            n <- uniform [1 .. 4]
            replicateM n (uniform [1 .. 6])

      -- There are about 8.5 bits of entropy contained in the distribution
      -- of results for this process.  That's the information that you would
      -- learn by knowing the exact outcome.
      entropy dist `shouldApprox` 8.462406251802479

      -- The values of this distribution tell us a lot about the result: how
      -- many d6 dice were rolled, and what the resulting numbers were, in what
      -- order.  We can use the Functor instance to consider only a subset of
      -- the information
      --
      -- For example, suppose we don't care about the order of the dice.  We can
      -- remove that information from the distribution by sorting the dice, so
      -- that rolls with different orders are indistinguishable.
      entropy (sort <$> dist) `shouldApprox` 6.8224643388490005

      -- Often we don't even care about the individual dice at all, and just
      -- want to add them up and get a total.  This eliminates even more
      -- information.
      --
      -- In this distribution, the possible values range from 1 to 24.  You
      -- might expect the entropy to be log_2 24.  It's lower than that, though,
      -- because the outcomes don't have equal probability.
      entropy (sum <$> dist) `shouldApprox` 4.192258630390927

      -- We can also look at only how many of the d6 were rolled.  This entropy
      -- comes out to a remarkably round number!  The reason is simple: we're
      -- effectively looking at is the result of the first d4, which is
      -- unbiased.  There are 4 possibilities, so that's 2 bits of info.
      entropy (length <$> dist) `shouldApprox` 2

      -- There's a complex relationship between the number of d6 dice rolled and
      -- their sum.  If we learn the length, then, we learn a bit about the sum.
      -- Suppose we learn that the length is 1.  Perhaps we can hear the dice
      -- being rolled, but just cannot see the results on each die.  We might
      -- ask what the probability distribution is *now*.  This is a conditional
      -- distribution.
      let dist_1 = finiteConditional ((== 1) . length) dist

      -- One might as how much information we learned by learning the length,
      -- relative to the original distribution.  This calculation is known as
      -- relative entropy (also known as KL divergence).
      --
      -- In this case, we learned 2 bits of information (the result of the d4).
      relativeEntropy dist_1 dist `shouldApprox` 2

      -- But not all of that information is relevant to the sum!  We can also
      -- ask how much information we learned about the sum in particular.
      --
      -- We can determine that learning that only one die is rolled would give
      -- us about 1.44 bits of information about the sum.
      relativeEntropy (sum <$> dist_1) (sum <$> dist)
        `shouldApprox` 1.4445630236465163

      -- How much information are we still missing about the sum?  Well, if we
      -- know there was only one die rolled, then the information we're missing
      -- is what number came up on that die, and 100% of that info is relevant
      -- to the sum.  That's log_2 6 bits of information, or about 2.58.
      entropy (sum <$> dist_1) `shouldApprox` logBase 2 6
      entropy (sum <$> dist_1) `shouldApprox` 2.5849625007211565

      -- On the other hand, suppose we learn that there were 2, 3, or 4 d6 dice
      -- rolled. Then these results come out different, because knowing that
      -- more dice were rolled leaves a lot more room for uncertainty about the
      -- sum!  So knowing the length tells you less about the sum, and there is
      -- more entropy remaining in the conditional distribution of sums.
      --
      -- Interestingly, it is actually the two and three-roll cases that tell
      -- you the least about the sum.  They are near the middle, so their sums
      -- overlap the most with possible sums from other lengths, and knowing the
      -- number of rolls does less to narrow down the possibilities.  On the
      -- other hand, the remaining entropy is still higher with the four-roll
      -- case: although knowing that there are four rolls gives you more
      -- information, there's even more information to know!
      let dist_2 = finiteConditional ((== 2) . length) dist
      relativeEntropy dist_2 dist `shouldApprox` 2
      relativeEntropy (sum <$> dist_2) (sum <$> dist)
        `shouldApprox` 0.594873350537772
      entropy (sum <$> dist_2) `shouldApprox` 3.274401919288769

      let dist_3 = finiteConditional ((== 3) . length) dist
      relativeEntropy dist_3 dist `shouldApprox` 2
      relativeEntropy (sum <$> dist_3) (sum <$> dist)
        `shouldApprox` 0.48020133600720066
      entropy (sum <$> dist_3) `shouldApprox` 3.599291802435438

      let dist_4 = finiteConditional ((== 4) . length) dist
      relativeEntropy dist_4 dist `shouldApprox` 2
      relativeEntropy (sum <$> dist_4) (sum <$> dist)
        `shouldApprox` 0.9766227667617642
      entropy (sum <$> dist_4) `shouldApprox` 3.814117822165162

      -- A reasonable question to ask is this: without knowing the specific
      -- length, how many bits of extra information might we *expect* to learn
      -- about the sum from knowing the length.  This is known as mutual
      -- information: the number of bits of *shared* information between the
      -- length and the sum.
      --
      -- Imagine a Venn-style diagram with two overlapping circles.  The entire
      -- diagram represents the information you'd gain from knowing an exact
      -- outcome.  One circle represents the information you'd gain from knowing
      -- the length, and the other represents the information you'd gain from
      -- knowing the sum.  The two circles overlap, and the overlap represents
      -- the mutual information between the two.
      --
      -- So it answers the question we asked: "If I know the length, how many
      -- bits of info does that tell me about the sum?" It also answers the
      -- converse question: "If I know the sum, how many bits of info does that
      -- tell me about the length?"
      mutualInformation length sum dist `shouldApprox` 0.8740651192382936
      mutualInformation sum length dist `shouldApprox` 0.8740651192382936

      -- An observant reader might notice that the mutual information given
      -- above is the average of the relative entropies for the four possible
      -- lengths.  This is because all four lengths are equally likely.  In
      -- general, the mutual information is the *expected* relative entropy
      -- of the conditional distribution versus the original.  That is:
      let expectedRelativeEntropy = expectation $ do
            x <- length <$> dist
            let cond = finiteConditional ((== x) . length) dist
            return $ relativeEntropy (sum <$> cond) (sum <$> dist)

      mutualInformation length sum dist `shouldApprox` expectedRelativeEntropy

  describe "bayesian" $ do
    -- We have a coin that might be fair (equal chance of heads or tails), or
    -- might be biased (0.75 chance of heads, 0.25 chance of tails).  As a
    -- prior, each is equally likely.
    let model = bernoulli :: Rational -> ExactDist Bool
        prior = uniform [0.75, 0.5]

    it "works in general" $ do
      -- We will flip the coin once, and update our estimate of the chance the
      -- coin is fair.
      let ifHeads = bayesian model (== True) prior
          ifTails = bayesian model (== False) prior

      -- If the coin comes up heads, we estimate it is the biased one with a
      -- 3/5 probability.
      --
      -- By Bayes' Law, P(B|H) = P(H|B) * P(B)  / P(H)
      --                       = (3/4)  * (1/2) / (1/2 * 3/4 + 1/2 * 1/2)
      --                       = 3/5
      approxProbability epsilon (> 0.5) ifHeads `shouldApprox` (3 / 5)

      -- If the coin comes up tails, we estimate it is the biased one with a
      -- 1/3 probability.  This is slow, so we choose a smaller epsilon.
      approxProbability 0.01 (> 0.5) ifTails `shouldSatisfy` \x -> abs (x - 1 / 3) < 0.01

    it "works exactly with finite version" $ do
      let ifHeads = finiteBayesian model (== True) prior
          ifTails = finiteBayesian model (== False) prior
      probability (> 0.5) ifHeads `shouldBe` (3 / 5)
      probability (> 0.5) ifTails `shouldBe` (1 / 3)
