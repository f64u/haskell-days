module Counter where

import qualified Data.Map.Strict               as Map

type Counter c = Map.Map c Integer

counterize :: (Ord a, Foldable f) => f a -> Counter a
counterize = foldr (\x -> Map.unionWith (+) (Map.singleton x 1)) Map.empty
