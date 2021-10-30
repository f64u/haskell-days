module WordSegmenterLib where
import qualified Data.Set                      as Set

import           Debug.Trace

-- | Segments words based on their occurance in dict. 
--   Returns prematurely if it compeletely consumed the string.
segmentWords :: Set.Set String -> String -> ([String], String)
segmentWords dict str =
  let (lst, word, _) = segmentNextCharInWord ([], "", str) in (lst, word)
 where
  segmentNextCharInWord
    :: ([String], String, String) -> ([String], String, String)
  segmentNextCharInWord (lst, word, "") = (lst, word, "") -- word was not found and string exhausted, we did what we could :(
  segmentNextCharInWord (lst, word, c : rest)
    | length newWord >= 24  -- largest word in dict
    = segmentNextCharInWord (lst, newWord ++ rest, "")
    | otherwise
    = let (lst', word', rest') = segmentNextCharInWord (lst, newWord, rest)
      in  case (null word', null rest') of
            (True, True) -> (lst', "", "") -- god is good
            (False, True) | newWord `elem` dict ->
              segmentNextCharInWord (lst ++ [newWord], "", rest)
            _ -> (lst', word', "")
    where newWord = word ++ [c]
