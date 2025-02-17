module Ch07_IO.Exercises.IOCalculator where

import Control.Arrow
import Data.List.Split
import Prelude

-- | Attempts to map @[String]@ to a @[Int]@.
mapToNums :: [String] -> [Int]
mapToNums = (read @Int <$>)

-- | Splits an input string at white-space.
splitExpr :: String -> [String]
splitExpr = splitOn " "

-- | Performs a sum in IO. Asks the user for 2 numbers separated by a white-space. Computes their sum.
runSum :: IO ()
runSum = do
    input <- getLine
    print $ sum $ (mapToNums <<< splitExpr) input

-- | Maps the operator symbol from the user input to the appropriate arithmetic function. Computes the result.
evalExpr :: [String] -> Int
evalExpr expr = case expr of
    "+" : _ -> sum nums
    "-" : _ -> foldl (-) 0 nums
    "*" : _ -> product nums
    _ -> error "not supported"
  where
    nums = mapToNums $ tail expr

{- | Runs @evalExpr@ in IO. Asks the user for a prefix expression and evaluates it in IO. The input expression
is presumeed to be in the correct format.
-}
runCalc :: IO ()
runCalc = do
    expr <- splitExpr <$> getLine
    print $ evalExpr expr
