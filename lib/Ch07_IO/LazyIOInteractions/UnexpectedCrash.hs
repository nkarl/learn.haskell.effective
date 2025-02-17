module Ch07_IO.LazyIOInteractions.UnexpectedCrash where

import Control.Arrow
import Prelude

{-
    NOTE: in this example we have a program that creates a lot of files, each has some data to read.
        the program create the files, read them and write their content to the screen.

    relevant functions:
        - readFile
        - writeFile
        - mapM
-}

makeAndReadFile :: Int -> IO String
makeAndReadFile fileUID = writeFile fileName fileName >> readFile fileName
  where
    fileName = "/tmp/test" <> show fileUID

{- | This function will crash because of interaction between IO and lazyness.

1. when we call @readFile@, we are not actually reading anything.
    In fact, what happened is that we created an unevaluated expression.
    This expression
        - represents the contents of the file, and
        - associates the contents with _a real-world state_ where the file has presumably been opened.
    _No work_ has been done yet.

    Essentially, the program has only built a graph of the functions. This _is_ lazyness.

2. when we bind @showFiles@ to the expression @show >>> putStrLn@, the function @show@ now needs to evaluate
    all the Strings. _Now_ the work actually gets done.

3. at the same time, @readFile@ doesn't close the file handle until the contents of the file have been read.
    Thus, we end up spawning more handles than the system allows. We essentially tried to open _everything_
    before showing anything.
-}
unsafe :: IO ()
unsafe = (putStrLn <<< show) =<< showFiles
  where
    showFiles = mapM makeAndReadFile [1 .. 5_000_000] :: IO [String]

makeAndShow :: Int -> IO ()
makeAndShow uid = makeAndReadFile uid >>= putStrLn

{- | we use @foldl@ to fix the problem with opening _everything at once_. The @foldl@ function essentially
forces a sequential evaluation. That is, the callback function must be evaluated for each @uid@ before moving
on to the next.
-}
safe :: IO ()
safe =
    foldl
        ( \io uid -> io >> makeAndShow uid
        )
        (pure ())
        [1 .. 5_000_000]
