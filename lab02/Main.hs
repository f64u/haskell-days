module Main where

import System.IO
import Data.List
import Interval
import SimpleTime

-- You can change EndpointType to Int for testing, but make sure to change it back
-- before submission.
type EndpointType = Time
-- type EndpointType = Int


---- Input/Output Interface ----

-- Example: splitOn ',' "abc,def,ghi"  => ["abc", "def", "ghi"]
splitOn :: Char -> String -> [String]
splitOn splitChar []    = [[]]
splitOn splitChar (headChar:restChars)
  | splitChar == headChar = [[]] ++ splitOn splitChar restChars
  | otherwise             = (headChar:currentWord):restWords
  where
    currentWord:restWords = splitOn splitChar restChars

-- Vanilla Haskell doesn't allow instances on type synonyms,
-- so we can't make customized Show/Read instances.

readIS :: String -> IntervalSet EndpointType
readIS = map read . splitOn ','

showIS :: IntervalSet EndpointType -> String
showIS = concat . intersperse "," . map show . normalizeIS

-- Combine touching/overlapping regions and remove empty intervals.
-- Inverting twice effectively combines overlapping regions.

normalizeIS :: (Ord a, Bounded a) => IntervalSet a -> IntervalSet a
normalizeIS s
  | simplified == [] = [Range minBound minBound]
  | otherwise        = simplified
  where
    inverse = difference allIS
    simplified = sort . removeEmptyIntervals . inverse . inverse $ s

processLine :: String -> String
processLine line =
  case words line of
    "intersection":rest -> undefined -- hint: These can each be done in one line
    "union":rest        -> undefined -- further hint: think map and ($) and (.)
    "difference":rest   -> undefined
    "disjoint":rest     -> undefined
    "equal":rest        -> undefined
    _                   -> "Invalid input"

main :: IO ()
main = do
  -- <--- Write code here to get a line from the input
  case undefined of -- Examine the line
    'q':_ -> return () -- "q" or "quit" to quit
    _     -> do
      -- <--- Process and output the line here
      hFlush stdout -- "Flush" the output buffer (otherwise Windows may not show output)
      atEnd <- isEOF -- Check if at the end of a file before continuing (for testing)
      if atEnd then
        return ()
      else
        main -- Repeat
