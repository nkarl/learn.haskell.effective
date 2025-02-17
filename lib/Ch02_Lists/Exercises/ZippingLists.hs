{-# LANGUAGE ParallelListComp #-}

module Ch02_Lists.Exercises.ZippingLists where

import Prelude hiding (zipWith)

{-
    NOTE: Problem 2. Implement the function `zipWith`
    1. Implement it with and without using list comprehension.
    2. Can you implement it using `foldl`?

    The function `zip` is a special case of `zipWith`. `zipWith` combines 2 lists "with"
    a function.

    @
    let zip' = zipWith (,)
    -- >>> zip' [1..] [5,4..1]
    [(1,5), (2,4), (3,3), (4,2), (5,1)]
    @
-}

-- | Combines 2 lists according to a function. Uses explicit recursion.
zipWith :: forall a b c. (a -> b -> c) -> [a] -> [b] -> [c]
zipWith f as bs =
    let
        a = head as
        b = head bs
     in
        if null as || null bs
            then mempty
            else f a b : zipWith f (tail as) (tail bs)

{- | Combines 2 lists according to a function. Uses list comprehension.

Requires _Parallel List Comprehension_ language pragma.

@
{\-# LANGUAGE ParallelListComp #-\}
@
-}
zipWith' :: forall a b c. (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f as bs =
    [ f a b
    | a <- as
    | b <- bs
    ]
