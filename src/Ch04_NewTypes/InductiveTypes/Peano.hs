module Ch04_NewTypes.InductiveTypes.Peano where

import Prelude

-- | A Peano number is either a \"Zero\" or a "Successor to some Peano number".
data Peano
    = Z
    | S Peano
    deriving (Show)

-- | Converts an Integer to a Peano type. A number \`n\` is always a successor of some Peano variant.
toPeano :: Integer -> Peano
toPeano 0 = Z
toPeano n = S (toPeano $ n - 1) -- not lazy, will always recur to the bottom, _and then_ build up a nested Peano structure.

-- | Converts a Peano type to an Integer.
fromPeano :: Peano -> Integer
fromPeano Z = 0
fromPeano (S p) = succ (fromPeano p) -- not lazy, will always recur to the bottom, _and then_ pop a stack for each `succ` on the Integer value.

-- | Compares 2 Peano structures.
eqPeano :: Peano -> Peano -> Bool
eqPeano p p' = case (p, p') of
    (Z, Z) -> True
    (S n, S n') -> eqPeano n n'
    _ -> False

addPeano :: Peano -> Peano -> Peano
addPeano Z b = b
addPeano (S a) b = addPeano a (S b)
