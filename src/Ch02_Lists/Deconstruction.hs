module Ch02_Lists.Deconstruction where

import Prelude hiding (foldl, foldr)

-- | Checks if the parentheses in a String are balanced, i.e. it has the same nunber of opening and closing parentheses.
isBalanced :: String -> Bool
isBalanced str =
    0 == check 0 str
  where
    check :: Integer -> String -> Integer
    check count [] = count
    check count (x : xs)
        | x == '(' = check (count + 1) xs
        | x == ')' = check (count - 1) xs
        | otherwise = check count xs

isBalanced' :: String -> Bool
isBalanced' str =
    0 == foldl check 0 str
  where
    check :: Integer -> Char -> Integer
    check count x
        | x == '(' = count + 1
        | x == ')' = count - 1
        | otherwise = count

{- | Takes a function, an initial state, and a list. Applies the function to each element in the list, reducing it to a new state.

Applies the function `f` _before_ recursing.
-}
foldl :: forall a b. (a -> b -> a) -> a -> [b] -> a
foldl _ state [] = state
foldl f state (x : xs) = foldl f (f state x) xs

{- | Takes a function, an initial state, and a list. Applies the function to each element in the list, reducing it to a new state.

Applies the function `f` _after_ recursing to the bottom.
-}
foldr :: forall a b. (b -> a -> a) -> a -> [b] -> a
foldr _ state [] = state
foldr f state (x : xs) = f x (foldr f state xs)

foldr' :: forall a b. (b -> a -> a) -> a -> [b] -> a
foldr' f state xs =
    if null xs
        then state
        else f (head xs) (foldr' f state (tail xs))
