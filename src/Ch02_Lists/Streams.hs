module Ch02_Lists.Streams where

import Prelude

-- | Generates a type of list called a a _stream_ or _generator_. The tail of this list is a _thunk_.
numbersFrom :: Integer -> [Integer]
numbersFrom n =
    n : numbersFrom (n + 1)

radsToDegrees :: Float -> Integer
radsToDegrees rad = degrees !! converted
  where
    converted = truncate $ (rad * 360) / (2 * pi)
    degrees = cycle [0 .. 359]
