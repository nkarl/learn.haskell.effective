module Ch05_StructuringProjects.PhantomTypes where

import Data.Foldable (find)
import Prelude

{-
    NOTE: we need build a small model of an API to manage user profile information
    for a social media app.

    NOTE: Technical Requirement
    A user's email address is private information. It should not be on the same
    privilege plane as other data. This also applies to the user's password.
    These private information should only be available to a user who is logged in.

    NOTE: We can use the phantom types to represent this. It's a different from
    sum types.
-}

data User = User
    { userName :: String
    , userInternetPoints :: Int
    , userPassword :: String
    , userEmailAddress :: String
    }

users :: [User]
users = [george, porter]
  where
    george =
        User
            { userName = "george"
            , userInternetPoints = 1000
            , userPassword = "secret"
            , userEmailAddress = "gbird2015@example.com"
            }
    porter =
        User
            { userName = "porter"
            , userInternetPoints = 500
            , userPassword = "hunter2"
            , userEmailAddress = "woofwoof@example.com"
            }

lookupUser :: String -> Maybe User
lookupUser name =
    find (\u -> userName u == name) users

getUserName :: User -> String
getUserName = userName

getUserScore :: User -> Int
getUserScore = userInternetPoints

getUserEmailAddress :: User -> String
getUserEmailAddress = userEmailAddress
