import System.IO

main :: IO ()
main = do
     putStr "> "
     hFlush stdout
     line <- getLine
     putStrLn line -- CHANGE ME
     main
