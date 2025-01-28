module Ch02_Lists.Streams where

import Prelude hiding (cycle)

-- | Generates a type of list called a a _stream_ or _generator_. The tail of this list is a _thunk_.
numbersFrom :: Integer -> [Integer]
numbersFrom n =
    n : numbersFrom (n + 1)

{- | Always returns an Integer between 0 and 359 by using the @cycle@ function.

@
6 == radsToDegrees 0.12 -- >>> True
@
-}
radsToDegrees :: Float -> Integer
radsToDegrees rad = degrees !! degree
  where
    -- makes a circular list from a range [a, b] (inclusive)
    degrees = cycle [0 .. 359]
    -- converts grad to degree, truncating all decimal places.
    -- the degree is used as index for the cyclic list `degrees`.
    degree = truncate $ (rad * 360) / (2 * pi)

-- | Our own version of the `cycle` _generator_ function.
cycle :: forall a. [a] -> [a]
cycle input = helper input
  where
    -- at the end of input, we "cycle" back to the same list.
    helper [] = cycle input
    -- demarcates the head and the rest (a thunk).
    helper (x : xs) = x : helper xs

{- | Exactly the same as `cycle` but even shorter. We can do this because we know the structure of a list, which is a Semigroup.

A Semigroup means that the polymorphic type `a` contains a set of symbols and supports the associative operator `concat` (or `<>`).
-}
cycle' :: forall a. (Semigroup a) => a -> a
cycle' input = input <> cycle' input

_foldr :: forall a b. (b -> a -> a) -> a -> [b] -> a
_foldr _ state [] = state
_foldr f state (x : xs) = f x (_foldr f state xs)

findFirst :: (a -> Bool) -> [a] -> [a]
findFirst _ [] = []
findFirst predicate (x : xs)
    | predicate x = [x]
    | otherwise = findFirst predicate xs

-- | Computes a Fibonacii value at the given index. Becomes noticeably sluggish at around 20 or 30.
fib :: (Eq a, Num a) => a -> a
fib n
    | n == 0 = 0
    | n == 1 = 1
    | otherwise = fib (n - 1) + fib (n - 2)

-- | Creates a stream/generator of Fibonacci sequence. Mapped from the compute function `fib`.
fibs :: [Integer]
fibs = fib <$> [0 ..]

-- | Takes all Fibonacci values less than 100.
allFibsLessThan_100 :: [Integer]
allFibsLessThan_100 = takeWhile (< 100) fibs

-- | Takes all Fibonacci values less than 1_000_000. Runtime complexity depends on the efficiency of the compute function `fib`. Becomes noticeably sluggish at around `fib 20` or `fib 30`.
allFibsLessThan_1_000_000 :: [Integer]
allFibsLessThan_1_000_000 = takeWhile (< 1_000_000) fibs
