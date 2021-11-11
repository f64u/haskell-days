-- Main.hs for Lab 6
--
-- You should not need to modify this file for the regular lab.
--
-- In particular, the non-REPL branch of the main function needs
-- to remain unchanged so grading will work.
--
-- Haskeline lets you hit the up arrow in our program's REPL.
--
-- You may need to install it with:
--
-- $ cabal install haskeline
--
-- If Haskeline doesn't work, use the provided simple repl.
-- (See the comments in the main function, line 52.)
--
import           Eval
import           Lab6
import           System.Console.Haskeline
import           System.Environment
import           System.IO
import           Unparse


-- If second argument is Left, prefix the error message
-- with the first argument. Otherwise, pass-through.
prefixError :: String -> Either String a -> Either String a
prefixError prefix (Left errMsg) = Left (prefix ++ errMsg)
prefixError _      rightResult   = rightResult


tryParseEval :: String -> EvalResult
tryParseEval expStr =
    parse expStr >>= (prefixError "Evaluation error: " . eval)


-- Convert result to string or return error message.
showParseEval :: String -> String
showParseEval expStr =
    case tryParseEval expStr of
      Right result  -> show result
      Left errorMsg -> errorMsg


-- If standard input is a terminal, display a REPL.
-- Otherwise, just parse, eval, and show each line.
main :: IO ()
main = do
    isTerminal <- hIsTerminalDevice stdin
    args <- getArgs
    if isTerminal || ("--repl" `elem` args) then
        -- If Haskeline doesn't work, comment out this next line...
        runHaskeline
        -- ...and use this line instead:
        -- simpleRepl
    else
        -- If standard input is not a terminal, don't
        -- display the prompt and only display results.
        --
        -- This needs to be here for grading to work.
        getContents >>= mapM_ (putStrLn . showParseEval) . lines
    where
        runHaskeline = runInputT defaultSettings superRepl


-- Parses and evaluates given line.
--
-- For debugging, show the parses in
-- addition to the evaluation result.
--
-- If returns Nothing, it's time to quit.
handleReplLine :: String -> Maybe String
handleReplLine line =
    if line `elem` ["q", ":q", "quit", ":quit", "exit"] then
        Nothing
    else
        let
            unparsedStr =
                case parse line of
                    Right parsed -> unparse parsed ++ "\n"
                    _            -> ""
        in
        Just $
            (show . parse $ line) ++ "\n" ++
            unparsedStr ++ showParseEval line


-- Use if Haskeline doesn't work.
simpleRepl :: IO ()
simpleRepl = do
    putStr "> "
    hFlush stdout
    line <- getLine
    case handleReplLine line of
        Nothing        -> return ()    -- Time to quit.
        Just resultStr -> do
            putStrLn resultStr
            simpleRepl


-- Uses Haskeline so that you can hit the up arrow.
superRepl :: InputT IO ()
superRepl = do
    maybeLine <- getInputLine "> "
    case maybeLine of
        Nothing   -> return ()    -- Time to quit.
        Just line ->
            case handleReplLine line of
                Nothing        -> return ()    -- Time to quit.
                Just resultStr -> do
                    outputStrLn resultStr
                    superRepl
