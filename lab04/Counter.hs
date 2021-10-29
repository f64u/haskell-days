module Counter where

import qualified Data.Map.Strict               as Map

-- | A counter type holding a value and how many times it occured.
type Counter c = Map.Map c Integer

-- | Counts how many times a value occured in a Foldable structure.
counterize :: (Ord a, Foldable f) => f a -> Counter a
counterize = foldr (\x -> Map.unionWith (+) (Map.singleton x 1)) Map.empty
