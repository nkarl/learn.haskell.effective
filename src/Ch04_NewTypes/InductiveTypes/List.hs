{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Ch04_NewTypes.InductiveTypes.List where

import Control.Arrow
import Data.Char (toUpper)
import Prelude

data List a
    = Empty
    | Cons a (List a)
    deriving (Show)

-- | Converts from type @[a]@ to our custom type @List a@. Explicit recursion.
_toList :: forall a. [a] -> List a
_toList [] = Empty
_toList (x : xs) = Cons x (_toList xs)

{- |
Converts from type @[a]@ to our custom type @List a@. Uses @foldr@.

@
>>> toList [1,2,3]
Cons 1 (Cons 2 (Cons 3 Empty))

@
-}
toList :: forall a. [a] -> List a
toList = foldr Cons Empty

-- | Reverts from our custom type @List a@ to the type @[a]@. Explicit recursion.
_fromList :: forall a. List a -> [a]
_fromList Empty = []
_fromList (Cons x xs) = x : _fromList xs

{- |
Reverts from our custom type @List a@ to the type @[a]@. Uses a custom version of the @foldr@ pattern.

@
>>> fromList $ Cons 'a' (Cons 'b' (Cons 'c' Empty))
"abc"

@
-}
fromList :: forall a. List a -> [a]
fromList = listFoldr (:) []

listFoldr :: forall a s. (a -> s -> s) -> s -> List a -> s
listFoldr _ t Empty = t
listFoldr g t (Cons a as) = g a (listFoldr g t as)

listFoldl :: forall a s. (s -> a -> s) -> s -> List a -> s
listFoldl _ t Empty = t
listFoldl g t (Cons a as) = listFoldl g (g t a) as

listHead :: forall a. List a -> Maybe a
listHead Empty = Nothing
listHead (Cons a _) = Just a

listTail :: forall a. List a -> List a
listTail Empty = Empty
listTail (Cons _ as) = as

{- | Reverse a List, using a state accumulator.

>>> listReverse $ toList [1,2,3]
Cons 3 (Cons 2 (Cons 1 Empty))
-}
listReverse :: forall a. List a -> List a
listReverse xs = go xs Empty
  where
    go Empty acc = acc
    go (Cons a ls) acc = go ls (Cons a acc)

{- | Applies a unary function to a List.

1. Example
>>> listMap (* 5) $ toList [1, 2, 3]

2. Example
>>> listMap (toUpper) $ toList ['a', 'b', 'c']
-}
listMap :: forall a b. (a -> b) -> List a -> List b
listMap g = listFoldr (Cons <<< g) Empty
