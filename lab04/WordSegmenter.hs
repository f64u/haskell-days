module Main where

import           Data.Char                      ( toLower )
import           System.IO
import           WordSegmenterLib

main = do
  contents <- readFile "/usr/share/dict/words"
  getContents
    >>= putStrLn
    .   unwords
    .   segmentWords (map (map toLower) (words contents))
    .   init -- ooh \n is there didn't know



