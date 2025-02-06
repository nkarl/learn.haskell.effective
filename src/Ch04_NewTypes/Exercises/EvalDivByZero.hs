module Ch04_NewTypes.Exercises.EvalDyvByZero where

import Ch04_NewTypes.CalculatorApp qualified as Calc
import Prelude

{-
    NOTE: Write a new version of the `evaluate` function, named `safeEvaluate` that returns an error
    if the user tries to divide by zero.
-}

{- | Handles the division by zero case.c:w

>>> safeEvaluate $ Calc.Div (Calc.Lit 1) (Calc.Lit 0)
Left "Error: division by zero."
-}
safeEvaluate :: Calc.Expr -> Either Calc.ErrorMsg Int
safeEvaluate expr =
    case expr of
        Calc.Lit num -> Right num
        Calc.Add a b -> Right $ apply (+) a b
        Calc.Sub a b -> Right $ apply (-) a b
        Calc.Mul a b -> Right $ apply (*) a b
        Calc.Div a b -> case b of
            Calc.Lit 0 -> Left "Error: division by zero."
            _ -> Right $ apply div a b
  where
    apply :: (Int -> Int -> Int) -> Calc.Expr -> Calc.Expr -> Int
    apply op a b = op (Calc.evaluate a) (Calc.evaluate b)
