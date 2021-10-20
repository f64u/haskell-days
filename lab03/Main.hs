import           Data.Ratio
import           Lab3
import           System.IO

main :: IO ()
main = do
  putStr "> "
  hFlush stdout
  line <- getLine
  putStrLn . showR . eval . parse . tokenize $ line
  main

 where
  showR ratio = (show . numerator) ratio ++ " / " ++ (show . denominator) ratio
