import           Data.Char                      ( toLower )
import           Data.List
import qualified Data.Map.Strict               as Map

isWordChar :: Char -> Bool
isWordChar = (`elem` "\'-" ++ ['a' .. 'z'])


splitIntoWords :: String -> [String]
splitIntoWords = filter (not . null) . splitIntoWordsNoisy
 where
  splitIntoWordsNoisy s =
    let (before, after) = span isWordChar s
    in  before : if (not . null) after
          then splitIntoWordsNoisy (tail after)
          else []

counterize :: [String] -> Map.Map String Integer
counterize = foldr (\s -> Map.unionWith (+) (Map.singleton s 1)) Map.empty

main :: IO ()
main =
  getContents
    >>= mapM_ (\(k, v) -> putStrLn (k ++ " " ++ show v))
    .   Map.assocs
    .   counterize
    .   splitIntoWords
    .   map toLower
