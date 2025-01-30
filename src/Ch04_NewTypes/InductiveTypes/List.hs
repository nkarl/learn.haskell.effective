{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use foldr" #-}
module Ch04_NewTypes.InductiveTypes.List where

import Prelude

data List a
    = Empty
    | Cons a (List a)
    deriving (Show)

-- | Converts from type @[a]@ to our custom type @List a@. Explicit recursion.
_toList :: [a] -> List a
_toList [] = Empty
_toList (x : xs) = Cons x (_toList xs)

{- |
Converts from type @[a]@ to our custom type @List a@. Uses @foldr@.

@
>>> toList [1,2,3]
Cons 1 (Cons 2 (Cons 3 Empty))

@
-}
toList :: [a] -> List a
toList = foldr Cons Empty

-- | Reverts from our custom type @List a@ to the type @[a]@. Explicit recursion.
_fromList :: List a -> [a]
_fromList Empty = []
_fromList (Cons x xs) = x : _fromList xs

{- |
Reverts from our custom type @List a@ to the type @[a]@. Uses a custom version of the @foldr@ pattern.

@
>>> fromList $ Cons 'a' (Cons 'b' (Cons 'c' Empty))
"abc"

@
-}
fromList :: List a -> [a]
fromList = listFoldr (:) []

listFoldr :: (a -> s -> s) -> s -> List a -> s
listFoldr _ t Empty = t
listFoldr g t (Cons a as) = g a (listFoldr g t as)

listFoldl :: (s -> a -> s) -> s -> List a -> s
listFoldl _ t Empty = t
listFoldl g t (Cons a as) = listFoldl g (g t a) as

listHead :: List a -> Maybe a
listHead Empty = Nothing
listHead (Cons a _) = Just a

listTail :: List a -> List a
listTail Empty = Empty
listTail (Cons _ as) = as

--listReverse :: List a -> List a
--listReverse = listFoldl (Cons) Empty

-- listmap
