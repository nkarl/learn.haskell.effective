module Ch07_IO.Exercises.WordReplacementTherapy where

import Control.Arrow
import Control.Monad (void)
import Data.List.Split
import Data.Maybe (fromMaybe)
import Data.Traversable (for)
import System.Directory
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

type HayStack = [String]
type Needle = String
type Replacement = String

replace :: HayStack -> Needle -> Replacement -> Maybe HayStack
replace haystack needle replacement = do
    for haystack (Just <<< findAndReplace)
  where
    findAndReplace word = if word == needle then replacement else word

{- | Runs the app, which does the following:

    1. open and read contents at the file already put at `/tmp/latin.txt`.
    2. replace every needle from the contents (haystack) and show on the screen.

SETUP:
    1. data: get either
        - some stock latin text here: https://loremipsum.io/
        - a big list of words here: https://github.com/dwyl/english-words/blob/master/words_alpha.txt
    2. generate a file at `/tmp/words.txt`
-}
runApp :: IO ()
runApp = do
    let path = "/tmp/words.txt"

    putStr "Please enter 3 arguments ( path , word, replacement ): "
    input <- getLine
    {--
        GHCI is weird. It adds extra special character for keys like BackSpace.

        TODO change the any @_@ placeholder back to @path@ for testing in GHCI.
    --}
    [_, needle, replacement] <- case splitArgs input of
        [] -> error "empty input"
        [_] -> error "not enough arguments"
        (_ : needle : replacement) -> pure (path : needle : replacement)
    isValidFile <- doesFileExist path
    void $
        if isValidFile
            then do
                let connector = " "
                haystack <- splitOn connector <$> readFile path
                print $ replace haystack needle replacement
            else
                runApp
