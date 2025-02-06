module Ch04_NewTypes.Exercises.PlantingTrees where

import Prelude

{-
    NOTE:
    - [x] Write the definition of the binary tree type, and then
    - [x] implement the 3 functions in the book.
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
data Tree a
    = Leaf
    | Branch (Tree a) a (Tree a)

{-
   NOTE: hence forth, the tree is constrained by the `Ord` type class.
-}

{- | Creates a @Tree@ from a @List@ of polymorphic type. The tree's ordering is constrained by the
@Ord@ type class. The elements are ordered monotonically from left to right, i.e. the "smaller"
element is on the left branch and the "larger" is on the right branch.

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

>>> flatten $ makeTree [8,2,4,11,3,7]
[2,3,4,7,8,11]
-}
flatten :: (Show a) => Tree a -> [a]
flatten t = go t []
  where
    go Leaf acc = acc
    go (Branch left a right) acc = go left [] <> (a : acc) <> go right []

{-
    Reference: https://www.anardil.net/2018/binary-tree-in-haskell.html

    NOTE: what am I trying to accomplish here?

    NOTE: I am trying to implement a pretty `show` for the type `Tree a`. The tree is drawn _side-way_
    LTR, i.e. the root node starts on the left edge and the inner layers propagated toward the right
    edge of the screen.

    This means that the depth of each node (and layer) coincides with how far from the left margin it
    will be. Every node will be on its own line, so the depth is consistently computed for every node.
    All nodes on the same layer have the same depth and thus will start on the same colum.

    This is actually a very clever way to represent a binary tree. The terminal screen draws text
    content from top to bottom and there is virtually no bound on the downward direction. The horizontal
    direction is bounded on the right edge of the screen; any line that exceeds the width of the screen
    is automatically truncated and the its remainder sent to the next line.

    This tracks with the chracteristics of a binary tree: its depth increases by $O(Ln(N))$ while
    its total number of nodes increases by $O(N^2)$.

    In other words, because the number of layers increases much more slowly, it makes sense to draw
    the tree LTR. The nodes (whose numbers increase much faGster) naturally "fall" down the terminal
    console.
-}

newtype LayerDepth = LayerDepth Int deriving (Show, Eq, Ord)
newtype LayerWidth = LayerWidth Int deriving (Show, Eq, Ord)

instance (Show a) => Show (Tree a) where
    show Leaf = ""
    show tree = showable layerDepth layerWidth tree
      where
        layerDepth = LayerDepth 0 -- the root node starts at the head of new line
        gap = 1 -- the gap between layers
        layerWidth = LayerWidth (findWidestKey tree + gap)

{- | Transforms a @Tree a@ into a prettified String representation.

The tree is recursively drawn LTR layer by layer, with nodes on the right branches drawn first, node
by node and from top to bottom.

>>> makeTree ["hello","world","!"]
<BLANKLINE>
        "world"
"hello"
        "!"

>>> makeTree [3, 1, 2, 7, 5, 4]
<BLANKLINE>
  7
    5
      4
3
    2
  1
-}
showable :: (Show a) => LayerDepth -> LayerWidth -> Tree a -> String
showable _ _ Leaf = ""
showable (LayerDepth d) (LayerWidth w) (Branch left a right) =
    offset_right <> "\n" <> offset_center <> offset_left
  where
    offset_left = showable (LayerDepth $ d + w) (LayerWidth w) left
    offset_right = showable (LayerDepth $ d + w) (LayerWidth w) right
    offset_center = replicate d ' ' <> show a

{- | Finds the key that is the widest when shown in String representation.

>>> findWidestKey $ makeTree ["hello","world","!"]
7
-}
findWidestKey :: (Show a) => Tree a -> Int
findWidestKey Leaf = 0
findWidestKey (Branch left a right) =
    let
        l = findWidestKey left
        r = findWidestKey right
        c = length $ show a
     in
        maximum [l, r, c]

-- | Shows a @Tree@ of monomorphic @String@ in a human readable form.
showStringTree :: Tree String -> String
showStringTree = show

-- | Adds an @Int@ element to a @Tree@ of monomorphic @Int@.
addElementToIntTree :: Tree Int -> Int -> Tree Int
addElementToIntTree = flip insertNode

-- | Checks if a node exists in a @Tree a@.
hasNode :: forall a. (Eq a, Ord a) => a -> Tree a -> Bool
hasNode _ Leaf = False
hasNode a (Branch left k right)
    | a == k = True
    | a < k = hasNode a left
    | otherwise = hasNode a right

{- | Checks if a monomorphic @Tree Int@ contains some @Int@ element.

>>> flip doesIntExist 3 $ makeTree [3, 1, 2, 7, 5, 4]
True
-}
doesIntExist :: Tree Int -> Int -> Bool
doesIntExist = flip hasNode
