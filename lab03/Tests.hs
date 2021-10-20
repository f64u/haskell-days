module Tests where

import           Lab3

import           Data.Ratio

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
expect1 funcName func input expectedOutput = if expectedOutput == actual
  then Success
  else
    Failure
    $  "Expected "
    ++ evaledStr
    ++ "\n  to be   "
    ++ show expectedOutput
    ++ "\n  but got "
    ++ show actual
 where
  actual    = func input
  evaledStr = funcName ++ " $ " ++ show input

-- This is where you add new test cases.
tests :: [TestResult]
tests =
  [ expect1 "eval" eval (Number (-3)) (-3 % 1)
  , expect1
    "eval"
    eval
    (Op '*' (Op '+' (Number 2) (Number (-6))) (Op '+' (Number 3) (Number 2)))
    (-20)
  , expect1 "eval" eval (Op '/' (Number 13) (Number 6)) (13 % 6)
  , expect1
    "eval"
    eval
    (Op '+' (Op '*' (Number 2) (Number 3)) (Op '/' (Number 13) (Number (-6))))
    (23 % 6)
  , expect1 "tokenize" tokenize "1+2"    [TInt 1, TOp '+', TInt 2]
  , expect1 "tokenize" tokenize "1 + 20" [TInt 1, TOp '+', TInt 2, TInt 0]
  , expect1 "tokenize" tokenize "1 * -2" [TInt 1, TOp '*', TOp '-', TInt 2]
  , expect1 "tokenize"
            tokenize
            "1 + 2 * 3 + 4"
            [TInt 1, TOp '+', TInt 2, TOp '*', TInt 3, TOp '+', TInt 4]
  , expect1 "tokenize"
            tokenize
            "1 * 2 + 3 + 4"
            [TInt 1, TOp '*', TInt 2, TOp '+', TInt 3, TOp '+', TInt 4]
  , expect1 "tokenize" tokenize "(1+2)"     [TParen [TInt 1, TOp '+', TInt 2]]
  , expect1 "tokenize" tokenize " (1 + 2 )" [TParen [TInt 1, TOp '+', TInt 2]]
  , expect1 "tokenize"
            tokenize
            "(1 * 2 + 3)"
            [TParen [TInt 1, TOp '*', TInt 2, TOp '+', TInt 3]]
  , expect1
    "tokenize"
    tokenize
    "(-10 + 2) * 5"
    [TParen [TOp '-', TInt 1, TInt 0, TOp '+', TInt 2], TOp '*', TInt 5]
  , expect1
    "tokenize"
    tokenize
    "5 * (-10 + (2 + 4) * 3)"
    [ TInt 5
    , TOp '*'
    , TParen
      [ TOp '-'
      , TInt 1
      , TInt 0
      , TOp '+'
      , TParen [TInt 2, TOp '+', TInt 4]
      , TOp '*'
      , TInt 3
      ]
    ]
  , expect1
    "tokenize"
    tokenize
    "5 * (-10 + (2 + 4) * 3) * (3 + 2)"
    [ TInt 5
    , TOp '*'
    , TParen
      [ TOp '-'
      , TInt 1
      , TInt 0
      , TOp '+'
      , TParen [TInt 2, TOp '+', TInt 4]
      , TOp '*'
      , TInt 3
      ]
    , TOp '*'
    , TParen [TInt 3, TOp '+', TInt 2]
    ]
  , expect1 "parse"
            parse
            [TInt 2, TOp '+', TInt 3]
            (Op '+' (Number 2) (Number 3))
  , expect1 "parse"
            parse
            [TInt 2, TOp '*', TInt 3]
            (Op '*' (Number 2) (Number 3))
  , expect1 "parse"
            parse
            [TInt 2, TOp '/', TInt 3]
            (Op '/' (Number 2) (Number 3))
  , expect1 "parse"
            parse
            [TInt 2, TOp '+', TInt 3, TOp '*', TInt 5]
            (Op '+' (Number 2) (Op '*' (Number 3) (Number 5)))
  , expect1 "parse"
            parse
            [TParen [TInt 2, TOp '+', TInt 3], TOp '*', TInt 5]
            (Op '*' (Op '+' (Number 2) (Number 3)) (Number 5))
  , expect1 "parse"
            parse
            [TInt 2, TOp '+', TOp '-', TInt 3]
            (Op '+' (Number 2) (Number (-3)))
  , expect1 "parse"
            parse
            [TInt 2, TOp '/', TInt 3, TOp '*', TOp '-', TInt 5, TInt 7]
            (Op '*' (Op '/' (Number 2) (Number 3)) (Number (-57)))
  , expect1
    "parse"
    parse
    [ TInt 1
    , TInt 0
    , TOp '*'
    , TParen
      [ TOp '-'
      , TInt 5
      , TOp '+'
      , TParen [TInt 5, TOp '*', TOp '-', TInt 1, TInt 0]
      , TOp '/'
      , TParen [TOp '-', TInt 5, TOp '*', TInt 1, TInt 0]
      , TOp '*'
      , TInt 8
      , TOp '+'
      , TParen [TInt 1, TInt 1, TOp '+', TOp '-', TInt 5]
      , TOp '+'
      , TParen [TInt 8, TInt 0, TOp '/', TInt 2, TOp '*', TInt 2]
      , TOp '+'
      , TInt 5
      ]
    ]
    (Op
      '*'
      (Number 10)
      (Op
        '+'
        (Number (-5))
        (Op
          '+'
          (Op
            '*'
            (Op '/'
                (Op '*' (Number 5) (Number (-10)))
                (Op '*' (Number (-5)) (Number 10))
            )
            (Number 8)
          )
          (Op
            '+'
            (Op '+' (Number 11) (Number (-5)))
            (Op '+'
                (Op '*' (Op '/' (Number 80) (Number 2)) (Number 2))
                (Number 5)
            )
          )
        )
      )
    ) -- let's go ham
  , expect1
    "parse"
    parse
    [ TInt 1
    , TOp '+'
    , TParen
      [ TInt 2
      , TOp '+'
      , TParen
        [ TInt 3
        , TOp '+'
        , TParen
          [ TInt 4
          , TOp '+'
          , TParen [TInt 5, TOp '+', TParen [TInt 6], TOp '+', TOp '-', TInt 5]
          , TOp '+'
          , TOp '-'
          , TInt 4
          ]
        , TOp '+'
        , TOp '-'
        , TInt 3
        ]
      , TOp '+'
      , TOp '-'
      , TInt 2
      ]
    , TOp '+'
    , TOp '-'
    , TInt 1
    ]
    (Op
      '+'
      (Number 1)
      (Op
        '+'
        (Op
          '+'
          (Number 2)
          (Op
            '+'
            (Op
              '+'
              (Number 3)
              (Op
                '+'
                (Op
                  '+'
                  (Number 4)
                  (Op '+'
                      (Op '+' (Number 5) (Op '+' (Number 6) (Number (-5))))
                      (Number (-4))
                  )
                )
                (Number (-3))
              )
            )
            (Number (-2))
          )
        )
        (Number (-1))
      )
    )
  ]

-- Inspect the below in GHCi.

-- DO NOT MODIFY BELOW THIS LINE IN YOUR SUBMISSION --

successes = filter isSuccess tests

failures = filter (not . isSuccess) tests

failureMessages = map message failures

results = (length successes, length failures, failureMessages)

showFailures :: IO ()
showFailures = mapM_ putStrLn failureMessages
