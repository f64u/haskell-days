module Main where

import           Data.ByteString.Lazy           ( intercalate )
import           Data.Char                      ( toLower )
import qualified Data.List                     as List
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set
import           OrderlessList                  ( OrderlessList(OrderlessList) )
import           WordUtil                       ( splitIntoWords )

type Table = Map.Map (OrderlessList Char) (Set.Set String)

-- | Turns a list of words into a map with the keys being the/a word and the values being
--   the words that are comprised of the same characters that comprised the key, aka a permutation of it.
collectPermutations :: [String] -> Table
collectPermutations = Map.unionsWith Set.union
  . map (\x -> Map.singleton (OrderlessList x) (Set.singleton x))

main :: IO ()
main =
  getContents
    >>= mapM_ (putStrLn . List.intercalate ", " . List.sort . Set.toList)
    .   List.sort
    .   filter ((> 1) . length)
    .   Map.elems
    .   collectPermutations
    .   splitIntoWords
    .   map toLower
