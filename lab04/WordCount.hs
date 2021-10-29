import           Counter                        ( counterize )
import           Data.Char                      ( toLower )
import           Data.List
import qualified Data.Map.Strict               as Map
import           WordUtil                       ( splitIntoWords )

main :: IO ()
main =
  getContents
    >>= mapM_ (\(k, v) -> putStrLn (k ++ " " ++ show v))
    .   Map.toAscList
    .   counterize
    .   splitIntoWords
    .   map toLower
