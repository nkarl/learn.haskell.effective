{-# OPTIONS_GHC -Wno-type-defaults #-}
module Main where

import qualified Ch04_NewTypes.Exercises.PlantingTrees as PlantingTrees

main :: IO ()
main = do
  print $ PlantingTrees.makeTree [3,1,2,7,5,4]
