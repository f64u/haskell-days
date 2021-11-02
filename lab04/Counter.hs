module Counter where

import qualified Data.Map.Strict               as Map

-- | A counter type holding a value and how many times it occured.
type Counter c = Map.Map c Integer

-- | Counts how many times a value occured in a Functor foldable structure.
counterize :: (Ord a, Functor f, Foldable f) => f a -> Counter a
counterize = Map.unionsWith (+) . fmap (flip Map.singleton 1)

