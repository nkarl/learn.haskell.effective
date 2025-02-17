{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use concatMap" #-}
module Ch02_Lists.Comprehension where

import Prelude

double :: [Integer]
double = [2 * num | num <- [0 .. 10]]

{-
list comprehension syntax:

[ <a functional expression> | <condition 1>, <condition 2>, ... ]
-}

doubleOdds :: [Integer]
doubleOdds = [2 * num | num <- [0 .. 10], odd num]

{- | Takes 2 lists and does 2 things:
1. filter the list `as` down to all elements that are also in the list `bs`.
2. filter only the odd elements of `bs`
3. make pairs of elements from the 2 filtered lists.
-}
pairs :: (Integral a) => [a] -> [a] -> [(a, a)]
pairs as bs =
    let as' = filter (`elem` bs) as
        bs' = filter odd bs
        mkPairs a = (a,) `map` bs'
     in concat (mkPairs `map` as')

pairs' :: forall a. (Integral a) => [a] -> [a] -> [(a, a)]
pairs' as bs = [(a, b) | a <- as, a `elem` bs, b <- bs, odd b]

{-
   EXAMPLE: FOOD BUDGETING
-}

-- | Takes a list of guests and a name, checks if the name is in the list.
checkGuestList :: forall t a. (Foldable t, Eq a) => t a -> a -> Bool
checkGuestList guests name =
    name `elem` guests

data Guest = Guest {name :: String, cost :: Float} deriving (Show, Eq)

-- | a list of friends and their individual food cost (for their favorite food)
foodCosts :: [(String, Double)]
foodCosts =
    [ ("David", 10.00)
    , ("George", 4.00)
    , ("Alice", 27.50)
    ]

partyBudget :: forall a b. (Num a) => (b -> Bool) -> [(b, a)] -> a
partyBudget isAttending =
    sum . (cost `map`) . filter (isAttending . name)
  where
    name = fst
    cost = snd

{-
   Expands `partyBudget` to ensure that guests can eat more than one dish.

   1. gets a list of guests and the food they request.
   2. `willEat` takes a guest's name and a food, checks if the guest wants to eat it.
   3. `foodCost` takes a food and returns its price.
-}
partyBudget' :: (Num a) => (t1 -> Bool) -> (t1 -> t2 -> Bool) -> (t2 -> a) -> [(t1, t2)] -> a
partyBudget' isAttending willEat foodCost guests =
    sum $
        [ foodCost food
        | name <- fst `map` guests
        , isAttending name
        , food <- snd `map` guests
        , willEat name food
        ]
