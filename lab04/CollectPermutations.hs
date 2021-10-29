import           Counter                        ( counterize )
import           Data.ByteString.Lazy           ( intercalate )
import           Data.Char                      ( toLower )
import qualified Data.List                     as List
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set
import           WordUtil                       ( splitIntoWords )

import           Debug.Trace

newtype OrderlessList a = OrderlessList { getOList :: [a] }

instance Foldable OrderlessList where
  foldr f a lst = foldr f a (getOList lst)

instance (Show a) => Show (OrderlessList a) where
  show = ("OrderlessList " ++) . show . getOList

instance Ord a => Eq (OrderlessList a) where
  lst1 == lst2 = counterize lst1 == counterize lst2
instance Ord a => Ord (OrderlessList a) where
  lst1 `compare` lst2 = counterize lst1 `compare` counterize lst2


type Table = Map.Map (OrderlessList Char) (Set.Set String)

collectPermutations :: [String] -> Table
collectPermutations = foldr
  (\x -> Map.unionWith Set.union
                       (Map.singleton (OrderlessList x) (Set.singleton x))
  )
  Map.empty

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
