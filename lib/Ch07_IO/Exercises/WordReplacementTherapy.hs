module Ch07_IO.Exercises.WordReplacementTherapy where

import Control.Arrow
import Control.Monad
import Data.List.Split
import Prelude

-- | Splits an input string into a list of arguments.
splitArgs :: String -> [String]
splitArgs [] =
    error "invalid input. empty input."
splitArgs s =
    case splitOn " " s of
        (a : b : c) -> dropEmpty $ a : b : c
        _ ->
            error "invalid input. unable to split arguments."
  where
    dropEmpty :: [String] -> [String]
    dropEmpty = filter (not <<< (== ""))

-- | Makes a file, writes some content into it. Finally, shows the the content.
makeAndShow :: String -> String -> IO ()
makeAndShow fileName content = do
    writeFile fileName content
    readFile fileName >>= (show >>> print)

type HayStack = String
type Needle = String
type Replacement = String

replace :: HayStack -> Needle -> Replacement -> IO ()
replace haystack needle replacement = do
    {--
       TODO: find the needle in the haystack.
       - search the haystack for the needle and replace it with the new one.
    --}
    pure ()

runApp :: IO ()
runApp = do
    -- TODO: create a test data file at "/tmp/wordreplace.txt"
    let fileName = "/tmp/wordreplace.txt"
        content = "hello world!"
    makeAndShow fileName content

    input <- getLine
    -- TODO: transform input
    let (path : needle : replacement) = splitArgs input
    print input
