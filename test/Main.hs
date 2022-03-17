{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad (replicateM, replicateM_)
import Data.List (sort)
import Dist
import Test.Hspec

-- | A probability distribution with Rational probabilities.  This is convenient
-- in testing so that we can check for exact values.
type ExactDist a = Dist Rational a

epsilon :: Fractional a => a
epsilon = 1e-10

approxEq :: (Ord a, Fractional a) => a -> a -> Bool
approxEq x y = abs (x - y) < epsilon

shouldApprox :: (Show a, Ord a, Fractional a) => a -> a -> Expectation
x `shouldApprox` y = x `shouldSatisfy` approxEq y

main :: IO ()
main = hspec $ do
  describe "standard distributions" $ do
    describe "categorical" $ do
      it "assigns the right probabilities" $ do
        let d = categorical [(1 / 2, 1), (1 / 3, 2), (1 / 6, 3)] :: ExactDist Int
        probability d (== 1) `shouldBe` 1 / 2
        probability d (== 2) `shouldBe` 1 / 3
        probability d (== 3) `shouldBe` 1 / 6

    describe "uniform" $ do
      it "assigns the right probabilities" $ do
        let d = uniform [1, 2, 3] :: ExactDist Int
        probability d (== 1) `shouldBe` 1 / 3
        probability d (== 2) `shouldBe` 1 / 3
        probability d (== 3) `shouldBe` 1 / 3

    describe "geometric" $ do
      it "assigns the right probabilities" $ do
        let d = geometric (1 / 3) [1 .. 10] :: ExactDist Int
        probability d (== 1) `shouldBe` 1 / 3
        probability d (== 2) `shouldBe` 2 / 9
        probability d (== 3) `shouldBe` 4 / 27

    describe "bernoulli" $ do
      it "assigns the right probabilities" $ do
        let d = bernoulli (1 / 3) :: ExactDist Bool
        probability d (== True) `shouldBe` 1 / 3
        probability d (== False) `shouldBe` 2 / 3

    describe "binomial" $ do
      it "assigns the right probabilities" $ do
        let d = binomial (1 / 3) 3 :: ExactDist Int
        probability d (== 0) `shouldBe` 08 / 27
        probability d (== 1) `shouldBe` 12 / 27
        probability d (== 2) `shouldBe` 06 / 27
        probability d (== 3) `shouldBe` 01 / 27

    describe "negativeBinomial" $ do
      it "assigns the right probabilities" $ do
        let d = negativeBinomial (1 / 3) 3 :: ExactDist Int
        approxProbability epsilon d (== 0) `shouldApprox` (1 / 27)
        approxProbability epsilon d (== 1) `shouldApprox` (2 / 27)
        approxProbability epsilon d (== 2) `shouldApprox` (8 / 81)
        approxProbability epsilon d (== 3) `shouldApprox` (80 / 729)

    describe "hypergeometric" $ do
      it "assigns the right probabilities" $ do
        let d = hypergeometric 10 5 3 :: ExactDist Int
        probability d (== 0) `shouldBe` 6 / 72
        probability d (== 1) `shouldBe` 30 / 72
        probability d (== 2) `shouldBe` 30 / 72
        probability d (== 3) `shouldBe` 6 / 72

    describe "poisson" $ do
      it "assigns the right probabilities" $ do
        let d = poisson 3 :: Dist Double Int
        approxProbability epsilon d (== 0) `shouldApprox` (1 / exp 3)
        approxProbability epsilon d (== 1) `shouldApprox` (3 / exp 3)
        approxProbability epsilon d (== 2) `shouldApprox` (4.5 / exp 3)
        approxProbability epsilon d (== 3) `shouldApprox` (4.5 / exp 3)

  describe "Dist" $ do
    it "can describe a stochastic process" $ do
      let d :: ExactDist [Int] = simplify $ do
            -- Roll d4, then roll that number of d6 and add the d6's together.
            n <- uniform [1 .. 4]
            replicateM n (uniform [1 .. 6])
      replicateM_ 1000 $ do
        xs <- sample d
        length xs `shouldSatisfy` (>= 1)
        length xs `shouldSatisfy` (<= 4)
        xs `shouldSatisfy` all (>= 1)
        xs `shouldSatisfy` all (<= 6)

  describe "info theory" $ do
    it "example" $ do
      -- Our fundamental unit of information theory will be the bit, and the
      -- *entropy* of a probability distribution tells us how many bits of
      -- information you would learn by knowing a value chosen from that
      -- distribution.
      --
      -- A choice between two options can be represented with one bit.
      entropy (uniform [0, 1] :: Dist Double Int) `shouldApprox` 1

      -- On the other hand, choosing between 32 options requires 5 bits.
      entropy (uniform [1 .. 32] :: Dist Double Int) `shouldApprox` 5

      -- The picture gets messier if the distribution is not uniform.  If
      -- you flip a biased coin that almost always comes up heads, then you
      -- nearly know the outcome before the coin flip even happens.  The entropy
      -- of this distribution is low.  You can also thing of entropy as a
      -- measure of uncertainty.
      entropy (bernoulli 0.999 :: Dist Double Bool)
        `shouldApprox` 0.011407757737461145

      -- We can explore more about information theory with a more advanced
      -- example.  We'll use the stochastic process from earlier, where we roll
      -- a d4, then roll that number of d6.
      let d :: Dist Double [Int] = simplify $ do
            n <- uniform [1 .. 4]
            replicateM n (uniform [1 .. 6])

      -- There are about 8.5 bits of entropy contained in the distribution
      -- of results for this process.  That's the information that you would
      -- learn by knowing the exact outcome.
      entropy d `shouldApprox` 8.462406251802479

      -- The values of this distribution tell us a lot about the result: how
      -- many d6 dice were rolled, and what the resulting numbers were, in what
      -- order.  We can use the Functor instance to consider only a subset of
      -- the information
      --
      -- For example, suppose we don't care about the order of the dice.  We can
      -- remove that information from the distribution by sorting the dice, so
      -- that rolls with different orders are indistinguishable.
      entropy (sort <$> d) `shouldApprox` 6.8224643388490005

      -- Often we don't even care about the individual dice at all, and just
      -- want to add them up and get a total.  This eliminates even more
      -- information.
      --
      -- In this distribution, the possible values range from 1 to 24.  You
      -- might expect the entropy to be log_2 24.  It's lower than that, though,
      -- because the outcomes don't have equal probability.
      entropy (sum <$> d) `shouldApprox` 4.192258630390927

      -- We can also look at only how many of the d6 were rolled.  This entropy
      -- comes out to a remarkably round number!  The reason is simple: we're
      -- effectively looking at is the result of the first d4, which is
      -- unbiased.  There are 4 possibilities, so that's 2 bits of info.
      entropy (length <$> d) `shouldApprox` 2

      -- There's a complex relationship between the number of d6 dice rolled and
      -- their sum.  If we learn the length, then, we learn a bit about the sum.
      -- Suppose we learn that the length is 1.  Perhaps we can hear the dice
      -- being rolled, but just cannot see the results on each die.  We might
      -- ask what the probability distribution is *now*.  This is a conditional
      -- distribution.
      let d_1 = finiteConditional ((== 1) . length) d

      -- One might as how much information we learned by learning the length,
      -- relative to the original distribution.  This calculation is known as
      -- relative entropy (also known as KL divergence).
      --
      -- In this case, we learned 2 bits of information (the result of the d4).
      relativeEntropy d_1 d `shouldApprox` 2

      -- But not all of that information is relevant to the sum!  We can also
      -- ask how much information we learned about the sum in particular.
      --
      -- We can determine that learning that only one die is rolled would give
      -- us about 1.44 bits of information about the sum.
      relativeEntropy (sum <$> d_1) (sum <$> d)
        `shouldApprox` 1.4445630236465163

      -- How much information are we still missing about the sum?  Well, if we
      -- know there was only one die rolled, then the information we're missing
      -- is what number came up on that die, and 100% of that info is relevant
      -- to the sum.  That's log_2 6 bits of information, or about 2.58.
      entropy (sum <$> d_1) `shouldApprox` logBase 2 6
      entropy (sum <$> d_1) `shouldApprox` 2.5849625007211565

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
      let d_2 = finiteConditional ((== 2) . length) d
      relativeEntropy d_2 d `shouldApprox` 2
      relativeEntropy (sum <$> d_2) (sum <$> d)
        `shouldApprox` 0.594873350537772
      entropy (sum <$> d_2) `shouldApprox` 3.274401919288769

      let d_3 = finiteConditional ((== 3) . length) d
      relativeEntropy d_3 d `shouldApprox` 2
      relativeEntropy (sum <$> d_3) (sum <$> d)
        `shouldApprox` 0.48020133600720066
      entropy (sum <$> d_3) `shouldApprox` 3.599291802435438

      let d_4 = finiteConditional ((== 4) . length) d
      relativeEntropy d_4 d `shouldApprox` 2
      relativeEntropy (sum <$> d_4) (sum <$> d)
        `shouldApprox` 0.9766227667617642
      entropy (sum <$> d_4) `shouldApprox` 3.814117822165162

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
      mutualInformation d length sum `shouldApprox` 0.8740651192382936
      mutualInformation d sum length `shouldApprox` 0.8740651192382936

      -- An observant reader might notice that the mutual information given
      -- above is the average of the relative entropies for the four possible
      -- lengths.  This is because all four lengths are equally likely.  In
      -- general, the mutual information is the *expected* relative entropy
      -- of the conditional distribution versus the original.  That is:
      let expectedRelativeEntropy = expectation $ do
            x <- length <$> d
            let cond = finiteConditional ((== x) . length) d
            return $ relativeEntropy (sum <$> cond) (sum <$> d)

      mutualInformation d length sum `shouldApprox` expectedRelativeEntropy
