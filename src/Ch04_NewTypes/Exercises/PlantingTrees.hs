module Ch04_NewTypes.Exercises.PlantingTrees where

import Prelude

{-
    NOTE:
    - [x] Write the definition of the binary tree type, and then
    - [ ] implement the 3 functions below.
-}

{- | This co-product type represents a binary tree. There are 2 data variants:
- @Leaf@ (or @Nil@) represents the edges of the tree.
- @Branch@ contains 3 pieces of information:
    - a value of some polymorphic type
    - 2 recursive sub trees representing the the two branches of the tree.

Technically, the provided tree definition has no constraints on its ordering. There is no distinction
between the Left and Right branches. Its nodes thus can be in any order, so the constructor itself is
runtime-polymorphic, i.e. every tree has to be constructed from scratch with the order of the nodes
specified by the user.

Hence, the tree's ordering is up to the reader to implement. For this exercise, I use the most popular
ordering, i.e. left-to-right increasing monotonicity.
-}
data BinaryTree a
    = Leaf
    | Branch (BinaryTree a) a (BinaryTree a)
    deriving (Show)

-- | a trivial alias in order to type less.
type Tree = BinaryTree

{-
   NOTE: hence forth, the tree is constrained by the `Ord` type class.
-}

{- | Creates a @Tree@ from a @List@ of polymorphic elements. The tree's ordering is constrained by the
@Ord@ type class. The elements increase monotonically from left to right, i.e. the "smallest" element
is on the left and the "largest" is on the right.

>>> makeTree [8,2,4,11,3,7]
Branch (Branch Leaf 2 (Branch (Branch Leaf 3 Leaf) 4 (Branch Leaf 7 Leaf))) 8 (Branch Leaf 11 Leaf)
-}
makeTree :: (Show a, Ord a) => [a] -> Tree a
makeTree = foldr insertNode Leaf . reverse

-- | insert a node into a @Tree@ following the LTR ordering.
insertNode :: (Show a, Ord a) => a -> Tree a -> Tree a
insertNode a Leaf = Branch Leaf a Leaf
insertNode a t@(Branch left x right)
    | a == x = t
    | a < x = Branch (insertNode a left) x right
    | otherwise = Branch left x (insertNode a right)

{- | Converts a @Tree@ into a flat monotonically increasing sequence.

>>> flatten $ Branch (Branch Leaf 2 (Branch (Branch Leaf 3 Leaf) 4 (Branch Leaf 7 Leaf))) 8 (Branch Leaf 11 Leaf)
[2,3,4,7,8,11]
-}
flatten :: (Show a) => Tree a -> [a]
flatten t = go t []
  where
    go Leaf acc = acc
    go (Branch left a right) acc = go left [] <> (a : acc) <> go right []

-- | Shows a @Tree@ of monomorphic @String@ in a human readable form.
showStringTree :: Tree String -> String
showStringTree = undefined

-- | Adds an @Int@ element to a @Tree@ of monomorphic @Int@.
addElementToIntTree :: Tree Int -> Int -> Tree Int
addElementToIntTree = undefined

-- | Checks if a monomorphic @Tree Int@ contains some @Int@ element.
doesIntExist :: Tree Int -> Int -> Bool
doesIntExist = undefined
