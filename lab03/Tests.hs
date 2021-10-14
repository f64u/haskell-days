module Tests where

import Lab3

data TestResult
    = Success
    | Failure String

isSuccess :: TestResult -> Bool
isSuccess Success = True
isSuccess _       = False

message :: TestResult -> String
message Success           = "Success!"
message (Failure message) = message

-- Test a function that takes one argument.
-- Usage: expect1 "myFunc" myFunc arg expectedOutput
expect1 :: (Show a, Show b, Eq b) => String -> (a -> b) -> a -> b -> TestResult
expect1 funcName func input expectedOutput =
    if expectedOutput == actual then
        Success
    else
        Failure $
            "Expected " ++ evaledStr ++
            "\n  to be   " ++ show expectedOutput ++
            "\n  but got " ++ show actual
    where
        actual    = func input
        evaledStr = funcName ++ " " ++ show input


-- This is where you add new test cases.
tests :: [TestResult]
tests =
    [ expect1 "eval" eval
        (Number (-3))
        (-3)
    , expect1 "eval" eval
        (Mult (Plus (Number 2) (Number (-6))) (Plus (Number 3) (Number 2)))
        (-20)
    , expect1 "eval" eval
        (Div (Number 13) (Number 6))
        2
    , expect1 "eval" eval
        (Plus (Mult (Number 2) (Number 3)) (Div (Number 13) (Number (-6))))
        4
    -- , expect1 "tokenize" tokenize
    --     "1+2"
    --     -- expectation here --
    -- , expect1 "tokenize" tokenize
    --     "1 + 20"
    --     -- expectation here --
    -- , expect1 "tokenize" tokenize
    --     "1 * -2"
    --     -- expectation here --
    -- , expect1 "tokenize" tokenize
    --     "1 + 2 * 3 + 4"
    --     -- expectation here --
    -- , expect1 "tokenize" tokenize
    --     "1 * 2 + 3 + 4"
    --     -- expectation here --
    -- , expect1 "tokenize" tokenize
    --     "(1+2)"
    --     -- expectation here --
    -- , expect1 "tokenize" tokenize
    --     " (1 + 2 )"
    --     -- expectation here --
    -- , expect1 "tokenize" tokenize
    --     "(1 * 2 + 3)"
    --     -- expectation here --
    -- , expect1 "tokenize" tokenize
    --     "(-10 + 2) * 5"
    --     -- expectation here --
    -- , expect1 "tokenize" tokenize
    --     "5 * (-10 + (2 + 4) * 3)"
    --     -- expectation here --
    -- , expect1 "tokenize" tokenize
    --     "5 * (-10 + (2 + 4) * 3) * (3 + 2)"
    --     -- expectation here --
    --
    -- And then add some tests for "parse" as well.
    --
    ]


-- Inspect the below in GHCi.

-- DO NOT MODIFY BELOW THIS LINE IN YOUR SUBMISSION --

successes       = filter isSuccess tests
failures        = filter (not . isSuccess) tests
failureMessages = map message failures

results =
    ( length successes
    , length failures
    , failureMessages
    )

showFailures :: IO ()
showFailures = mapM_ putStrLn failureMessages
