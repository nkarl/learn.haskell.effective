{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ch06_TypeClasses.HigherKindedType where

import Control.Arrow
import Data.Kind

-- | Converts a @List@ of showable elements to a comma-separated @String@.
_toCSV :: (Show a) => [a] -> String
_toCSV = dropLeadingComma <<< foldl addField ""
  where
    addField s a = s <> "," <> show a
    dropLeadingComma s = case s of
        ',' : s' -> s'
        _ -> s

-- | This is a more generic version that supports any @Foldable@ context. Annotated with kind signature for educational purpose.
toCSV :: forall (t :: Type -> Type) (a :: Type). (Foldable t, Show a) => t a -> String
toCSV = dropLeadingComma <<< foldl addField ""
  where
    addField s a = s <> "," <> show a
    dropLeadingComma s = case s of
        ',' : s' -> s'
        _ -> s
