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

    NOTE: We can use the phantom types to represent this. They are different from
    regular data types. Phantom types do not have value constructors.
-}

-- phantom types without data constructors
data Authenticated
data UnAuthenticated

data User a = User
    { userName :: String
    , userInternetPoints :: Int
    , userPassword :: String
    , userEmailAddress :: String
    }

users :: [User a]
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

-- | Looks up the username regardless of their Authenticated status.
lookupUser :: String -> Maybe (User a)
lookupUser name =
    find (\u -> userName u == name) users

-- | Gets the username regardless of their Authenticated status.
getUserName :: User a -> String
getUserName = userName

-- | Gets the user's score regardless of their Authenticated status.
getUserScore :: User a -> Int
getUserScore = userInternetPoints

-- | Gets the user's email only if they are authenticated.
getUserEmailAddress :: User Authenticated -> String
getUserEmailAddress = userEmailAddress
