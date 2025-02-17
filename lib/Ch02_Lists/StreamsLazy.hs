module Ch02_Lists.StreamsLazy where

import Prelude

-- | Creates a lazy generator/generator for a Fibonacci sequence. Very efficient compared to the non-lazy `fibs` function.
generator :: (Num a) => a -> a -> [a]
generator first second = first : generator second next
  where
    next = first + second

-- | A wrapper for the lazy stream/generator starting with the base cases 0 and 1. Runtime complexity depends on the efficiency of the generator function.
lazyFibs :: [Integer]
lazyFibs = generator 0 1

-- | Takes all Fibonacci values less than 1_000_000. Runtime complexity depends on the efficiency of the generator function. Very efficient and shows no performance hits even when taking while less than 1_000_000_000_000.
lazyAllFibsLessThan_1_000_000 :: [Integer]
lazyAllFibsLessThan_1_000_000 = takeWhile (< 1_000_000) lazyFibs
