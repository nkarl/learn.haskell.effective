module Ch04_NewTypes.Exercises.PlantingTrees where

import Prelude

{-
    NOTE: Write the definition of the binary tree type, and then implement the 3 functions below.
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

-- | a trivial alias in order to type less.
type Tree = BinaryTree

showStringTree :: Tree String -> String
showStringTree = undefined

addElementToIntTree :: Tree Int -> Int -> Tree Int
addElementToIntTree = undefined

doesIntExist :: Tree Int -> Int -> Bool
doesIntExist = undefined
