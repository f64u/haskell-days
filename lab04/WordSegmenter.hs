module Main where

import           Data.Char                      ( isSpace
                                                , toLower
                                                )
import qualified Data.Set                      as Set
import           WordSegmenterLib

main = do
  contents <- readFile "/usr/share/dict/words"

  getContents
    >>= putStrLn
    .   unwords
    .   fst
    .   segmentWords (Set.fromList (map (map toLower) (words contents)))
    .   filter (not . isSpace) -- there shouldn't be any but just in case (and for example there can be a newline at the end)




