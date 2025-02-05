module Ch04_NewTypes.Exercises.PlantingTrees where

import Prelude

{-
    NOTE:
    - [x] Write the definition of the binary tree type, and then
    - [ ] implement the 3 functions below.
-}

{- | This co-product type represents a binary tree. There are 2 data variants:
 - @Leaf@ (or @Nil@) represents the edges of the tree.
 - a @Branch@ contains 3 pieces of information:
     - a value of some polymorphic type
     - 2 recursive sub trees representing the left and right branches of a binary tree.
-}
data BinaryTree a
    = Leaf
    | Branch (BinaryTree a) a (BinaryTree a)
    deriving (Show)

-- | a trivial alias in order to type less.
type Tree = BinaryTree

-- | Creates a @Tree@ from a @List@ of elements.
make :: (Show a, Ord a) => [a] -> Tree a
make [] = Leaf
make [a] = Branch Leaf a Leaf
make (a : b : xs)
    | b <= a = Branch (make (b : xs)) a Leaf
    | otherwise = Branch Leaf a (make (b : xs))

-- | Converts a @Tree@ into a @Monoid@.
flatten :: (Show a) => Tree a -> [a]
flatten t = go t []
  where
    go Leaf acc = acc
    go (Branch left a right) acc = (a : acc) <> go left [] <> go right []

-- | Shows a @Tree@ of monomorphic @String@ in a human readable form.
showStringTree :: Tree String -> String
showStringTree = undefined

-- | Adds an @Int@ element to a @Tree@ of monomorphic @Int@.
addElementToIntTree :: Tree Int -> Int -> Tree Int
addElementToIntTree = undefined

-- | Checks if a monomorphic @Tree Int@ contains some @Int@ element.
doesIntExist :: Tree Int -> Int -> Bool
doesIntExist = undefined
