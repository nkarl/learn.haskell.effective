{-# LANGUAGE ExplicitForAll #-}

module Ch07_IO.LazyIOInteractions.NoRaisedError where

import Data.Kind
import Prelude

copyFile :: FilePath -> FilePath -> IO ()
copyFile src dst =
    readFile src `andThen` writeFile dst

andThen :: forall (a :: Type) (b :: Type). IO a -> (a -> IO b) -> IO b
andThen = (>>=)

noPassword :: FilePath -> IO String
noPassword path = case path of
    "/etc/passwd" -> newIO "hey, that's a secret!"
    fname -> readFile fname
  where
    newIO = pure

showFile :: FilePath -> IO ()
showFile path = noPassword path `andThen` putStrLn

{- | In this example, because the @>>@ combinator discards the output of the prior function, when combined
with lazyness, the division-by-zero expression is never evaluated (because it's never needed).
-}
lazyIODemo :: IO ()
lazyIODemo =
    let
        sayHello :: IO ()
        sayHello = putStrLn "Hello"
        raiseMathError :: IO Int
        raiseMathError = putStrLn "I'm part of raiseMathError" >> pure (1 `div` 0)
     in
        sayHello >> raiseMathError >> sayHello
