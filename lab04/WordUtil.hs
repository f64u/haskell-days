module WordUtil where

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
