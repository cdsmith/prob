# dice-simulator

This was a project Shae and I worked on Sunday just to do something together.
The original idea was to recreate anydice.com as a Haskell library.  It ended
up morphing into a more general probability monad, not too different from
https://hackage.haskell.org/package/probability

We wanted it to work for infinite distributions, though, where that library
doesn't.  For example, there's a common "exploding dice" rule in dice games,
where any time you roll high enough on a die, you get to reroll that die and
add the reroll to the total *in addition to* the original.  And the reroll
is also subject to the exploding rule, so in principle you could roll an
unbounded value, just with very low probability.  But once you've got
potentially infinite distributions, you have to be careful with the order of
traversal in `join`!  We decided to maintain the invariant that outcomes are
always stored in decreasing order of probability.

This doesn't work yet.  There's a failing example in `app/Main.hs`, where
computing the negative binomial distribution for 2 successes fails if the
probability of success is less than the reciprocal of the golden ratio (!!!)
I believe it should work, but I may be missing something.
