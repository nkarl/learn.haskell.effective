module Memes where

import Control.Monad (unless)
import Prelude

{- | https://x.com/Charles_Ntd/status/1885150701292056720

@succ@ until some developer have a different opinion? The total number of dev should be much less than 7 billion. So I'll just keep asking until I find one dev!

>>> untilADifferentOpinion ( == 7_000_000) 11
True
-}
untilADifferentOpinion :: forall a. (Integral a) => (a -> Bool) -> a -> Bool
untilADifferentOpinion f a
    | f a = True
    | otherwise = untilADifferentOpinion f (succ a)
