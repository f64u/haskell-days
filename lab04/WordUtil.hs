module WordUtil where

-- | Returns whether a character is a wordChar or not.
--   A wordChar is either the characters ' or - or a lowercase alphapetical letter.
isWordChar :: Char -> Bool
isWordChar = (`elem` "\'-" ++ ['a' .. 'z'])

-- | Splits a string separated with wordChar characters into a list of strings of the strings 
--   between those characters
splitIntoWords :: String -> [String]
splitIntoWords = filter (not . null) . splitIntoWordsNoisy
 where
  splitIntoWordsNoisy s =
    let (before, after) = span isWordChar s
    in  before : if (not . null) after
          then splitIntoWordsNoisy (tail after)
          else []
