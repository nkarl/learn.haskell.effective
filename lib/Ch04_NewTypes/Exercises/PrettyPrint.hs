module Ch04_NewTypes.Exercises.PrettyPrint where

import Ch04_NewTypes.CalculatorApp qualified as Calc
import Prelude

{-
    NOTE: write a `prettyPrint` function that returns a human readable String that shows the
    equational calculation as would be written by a human student.

    For example:

    >>> putStrLn $ prettyPrint $ Lit 5 `Add` Lit 10
    5 + 10 = 15

    TODO: need to recur and stack up the correct number of parentheses.
-}

prettyPrint :: Calc.Expr -> String
prettyPrint = undefined
