module Ch02_Lists.Deconstructing where

import Prelude

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
    0 == reduce check 0 str
  where
    check :: Integer -> Char -> Integer
    check count x
        | x == '(' = count + 1
        | x == ')' = count - 1
        | otherwise = count

-- | Takes a function, an initial state, and a list. Applies the function to each element in the list, reducing it to a new state.
reduce :: forall a b. (a -> b -> a) -> a -> [b] -> a
reduce _ state [] = state
reduce f state (x : xs) = reduce f newState xs
  where
    newState = f state x
