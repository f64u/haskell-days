import Lab3
import System.IO

main :: IO ()
main = do
  putStr "> "
  hFlush stdout
  line <- getLine
  putStrLn . show . eval . parse . tokenize $ line -- CHANGE ME
  main
