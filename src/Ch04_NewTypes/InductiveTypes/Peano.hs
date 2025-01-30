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
toPeano n = S (toPeano $ n - 1) -- lazy

-- | Converts a Peano type to an Integer.
fromPeano :: Peano -> Integer
fromPeano Z = 0
fromPeano (S p) = succ (fromPeano p) -- lazy
