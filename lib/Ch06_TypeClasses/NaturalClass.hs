module Ch06_TypeClasses.NaturalClass where

import Prelude

class (Show n, Eq n) => Natural n where
    add :: n -> n -> n
    mul :: n -> n -> n
    addId :: n
    mulId :: n

instance Natural Int where
    add = (+)
    mul = (*)
    addId = 0
    mulId = 1

