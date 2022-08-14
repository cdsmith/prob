# prob

[![CI](https://github.com/cdsmith/prob/actions/workflows/ci.yml/badge.svg)](https://github.com/cdsmith/prob/actions/workflows/ci.yml)

A monad for discrete probability distributions in Haskell.  The type
`Distribution prob a` represents a probability distribution over values of `a`,
with probability represented by the type `prob`.  The meaning is very similar to
https://hackage.haskell.org/package/probability. However, this implementation is
carefully designed to work with potentially infinite distributions and recursive
do blocks, which do not always work correctly there.

For example, the following definition:

``` haskell
rerollOnes = do
  x <- uniform [1 .. 6]
  if x == 1
    then rerollOnes
    else return x
```

is a perfectly good description of a stochastic process that rolls a die, but
rerolls the die indefinitely if it lands on a 1.  The distribution monad from
the `probability` package hangs when asked to describe this distribution:

```
ghci> rerollOnes
fromFreqs ^CInterrupted.
ghci> 
```

This library, by contrast, gives a productive answer.  Note, however, that it is
an infinite answer:

```
ghci> possibilities rerollOnes
[(0.16666666666666666,2), (2.7777777777777776e-2,2), (0.16666666666666666,3),
(4.629629629629629e-3,2), (2.7777777777777776e-2,3), (0.16666666666666663,4),
(7.716049382716048e-4,2), (4.6296296296296285e-3,3), (2.7777777777777776e-2,4),
(0.1666666666666666,5), (0.16666666666666677,6), (1.2860082304526747e-4,2),
(7.716049382716048e-4,3), (4.6296296296296285e-3,4) ,(2.777777777777777e-2,5),
(2.7777777777777797e-2,6), (2.143347050754458e-5,2), (1.2860082304526747e-4,3),
(7.716049382716047e-4,4), (4.6296296296296285e-3,5), (4.629629629629632e-3,6),
(3.572245084590763e-6,2), (2.143347050754458e-5,3), (1.2860082304526747e-4,4),
^CInterrupted.
ghci> 
```

Left to run long enough, the program will continue to emit possibilities, with
the sums converging toward the expected result of a 0.2 probability of rolling
each number from 2 through 6.  You can also use `finitize` to approximate the
result:

```
ghci> possibilities $ simplify (finitize 0.00001 d)
[(0.19999998015419398,2), (0.19999988092516383,3), (0.19999928555098298,4),
(0.19999571330589844,5),(0.20000514006376077,6)]
ghci> 
```

Internally, a value of type `Distribution prob a` is represented not as a list
of possibilities but as a binary decision tree.  This is a rich enough structure
to allow productive enumeration of all possibilities using a breadth-first
traversal, even if the process described is infinite and left recursive.
