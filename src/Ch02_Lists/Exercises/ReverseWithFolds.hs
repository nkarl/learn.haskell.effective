{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Collapse lambdas" #-}
{-# HLINT ignore "Use id" #-}
module Ch02_Lists.Exercises.ReverseWithFolds where

import Control.Arrow
import Prelude hiding (foldl, foldr, reverse)

{-
    NOTE: Problem 1. Reverse a List with Folds
    Implement a function that will reverse a list using both `foldl` and `foldr`.
    Which one is simpler? Why might one be more efficient than the other?

    Using `foldl` to reverse a list is much more simple than using `foldr`, even though `foldr` is considered the more general _fold_ operator.

    To implement `foldr` takes a lot more work. For now a crude summary is: `foldl` can be rewritten as a `foldr` but the state/accumulator is made into a function `f`.
    The function `g` (to be applied to each element of the input list) is partially applied inside another function `construct`, and then passed on to `f` via the
    reverse composition `>>>`. The rationale involves the _fusion property_ discussed in Hutton's linked paper below.

    TODO: Follow the links below for a detailed breakdown.

    - https://stackoverflow.com/a/26036320
    - https://wiki.haskell.org/Foldl_as_foldr_alternative
    - Graham Hutton's _Tutorial on the Universality and Expressiveness of Fold_
        - https://people.cs.nott.ac.uk/pszgmh/fold.pdf

    To see which one is more efficient, I need to set up benchmark. That requires some work. However, for now a crude summary is: thanks to laziness, foldr
    can give a function f its first argument right away and keep the rest of the input list as a thunk.

    - https://stackoverflow.com/a/3429693
-}

-- | reverses a list using `foldl`.
reverse :: forall a. [a] -> [a]
reverse = foldl (flip (:)) mempty

-- | reverse a list using `foldr`. A lot more involved.
reverse' :: forall a. [a] -> [a]
reverse' = myFoldl (flip (:)) mempty

-- | `foldl` rewritten in terms of `foldr` using a lambda `construct`.
myFoldl :: (s -> a -> s) -> s -> [a] -> s
myFoldl g acc xs = foldr construct (\acc' -> acc') xs acc
  where
    construct = \a f -> (`g` a) >>> f

{- | Takes s function, an initial state, and s list. Applies the function to each element in the list, reducing it to s new state.

Applies the function `f` _before_ recursing.
-}
foldl :: forall s a. (s -> a -> s) -> s -> [a] -> s
foldl _ t [] = t
foldl g t (a : as) = foldl g (g t a) as

{- | Takes a function, an initial state, and a list. Applies the function to each element in the list, reducing it to a new state.

Applies the function `f` _after_ recursing to and evaluating the bottom.
-}
foldr :: forall s a. (a -> s -> s) -> s -> [a] -> s
foldr _ t [] = t
foldr g t (a : as) = g a (foldr g t as)

flipFoldl :: forall t a. (t -> a -> t) -> [a] -> t -> t
flipFoldl _ [] = \t -> t
flipFoldl g (a : as) = \t -> _f (g t a)
  where
    _f = (flip <<< foldl) g as

-- using `foldlr`
flipFoldl' :: forall t a. (t -> a -> t) -> [a] -> (t -> t)
flipFoldl' g = foldr h (\t -> t)
  where
    h = \a f -> \t -> f (g t a)

myFlippedFoldl :: (t -> a -> t) -> t -> [a] -> t
myFlippedFoldl g = flip (foldr h (\t -> t))
  where
    h = \a f -> \t -> f (g t a)
