module Main where

import           Data.Char                      ( isSpace
                                                , toLower
                                                )
import           System.IO
import           WordSegmenterLib

main = do
  contents <- readFile "/usr/share/dict/words"
  getContents
    >>= putStrLn
    .   unwords
    .   segmentWords (map (map toLower) (words contents))
    .   filter (not . isSpace) -- there shouldn't be any but just in case (and for example there can be a newline at the end)



