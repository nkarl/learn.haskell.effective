module Ch05_StructuringProjects.SmartConstructors where

import Data.List (sort)
import Prelude hiding (minimum)

{-
    NOTE: we need to deal with a list of numbers. We need to regularly find the
    smallest element in the list. We need to handle it appropriately when the
    list is empty. We could use the `Prelude.minimum` function but that is a
    partial function; it will fail on an empty list.

    NOTE: we can write our own version of `minimum` if we are sure that we are
    always working with a list that:
    - has at least one element, and
    - will be sorted (lazy lists are disqualified)
-}

data SortedList where
    SortedList :: {getSorted :: [Int]} -> SortedList

-- | A smart constructor to handle empty lists and sort non-empty acyclic lists.
makeSortedList :: [Int] -> Maybe SortedList
makeSortedList [] = Nothing
makeSortedList numbers = Just $ SortedList (sort numbers)

minimum :: SortedList -> Int
minimum (SortedList numbers) = head numbers
