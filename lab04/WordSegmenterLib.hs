module WordSegmenterLib where

segmentWords :: [String] -> String -> ([String], String)
segmentWords dict str =
  let (lst, word, _) = segmentNextCharInWord ([], "", str) in (lst, word)
 where
  segmentNextCharInWord
    :: ([String], String, String) -> ([String], String, String)
  segmentNextCharInWord (lst, word, "") = (lst, word, "") -- word was not found
  segmentNextCharInWord (lst, word, c : rest)
    | length newWord >= 24  -- largest word in dict
    = segmentNextCharInWord (lst, newWord ++ rest, "")
    | newWord
      `elem` dict
      &&     (  length newWord
             /= 1 {- a lot of single-letter words is dict??? -}|| newWord
             == "a"
             )
    = let (lst', word', rest') =
            segmentNextCharInWord (lst ++ [newWord], "", rest)
      in  if null word' && null rest'
            then (lst', "", "") -- god is good
            else segmentNextCharInWord (lst, newWord, rest)
    | otherwise
    = segmentNextCharInWord (lst, newWord, rest)
    where newWord = word ++ [c]
