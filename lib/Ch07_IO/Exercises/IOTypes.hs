{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Collapse lambdas" #-}
module Ch07_IO.Exercises.IOTypes where

import Prelude

myFunc :: IO (IO String)
myFunc = pure $ pure "Hello"

{- | when used with @>>=@ the function @myFunc@ needs to forward to @id@ before being binded to @putStrLn@.

The compiler suggests the @join@ function to flatten the duplicate contexts, monad is discussed in Chapter 9.
-}
useWithBind :: IO ()
useWithBind = myFunc >>= \io -> io >>= putStrLn

unpackIO :: IO (IO a) -> IO a
unpackIO io = io >>= id

funcB :: a -> [IO a]
funcB a = [pure a]

{-
    NOTE: this exercise part 3.
        Whenever we need to do many IO-bound computations at the same time, and at the end need to collect the results into
        a list for further actions (for example, reduce).
        the @foldl@ function ensures each IO-bound computation is evaluated before it is collected into an IO list.
-}

-- | This function is the same as @funcD@ and @funcE@, just with different syntax. Uses the @do@ notation.
funcC :: [IO a] -> IO [a]
funcC = foldl go (pure mempty)
  where
    go :: IO [a] -> IO a -> IO [a]
    go io_as io_a = do
        as <- io_as
        a <- io_a
        pure $ a : as

-- | Uses explicit lambda with @do@ notation at the second parameter.
funcD :: [IO a] -> IO [a]
funcD = foldl go (pure mempty)
  where
    go :: IO [a] -> IO a -> IO [a]
    go =
        \io_as -> \io_a -> do
            as <- io_as
            a <- io_a
            pure $ a : as

-- | Uses explicit lambda with the bind function @>>=@.
funcE :: [IO a] -> IO [a]
funcE = foldl go (pure mempty)
  where
    go :: IO [a] -> IO a -> IO [a]
    go =
        \io_as io_a ->
            io_as >>= \as ->
                io_a >>= \a ->
                    pure $ a : as
