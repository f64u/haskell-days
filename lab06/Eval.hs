module Eval
  ( EvalResult
  , eval
  ) where

import qualified Data.Map.Strict               as M

import           Control.Applicative
import           Data.Maybe                     ( fromJust )
import           Lab6

-- | Possible types in our evaluator, without accounting for errors
--   (`Either` could have replaced this, but I keep forgetting order)
data EvalValue = NumberValue Number | BoolValue Bool deriving (Eq, Ord)
instance Show EvalValue where
  show (NumberValue n) = show n
  show (BoolValue   b) = show b

-- Error message or result number.
type EvalResult = Either String EvalValue

type Bindings = [(Name, EvalValue)]


-- Return wrapped list of bare results if all inputs are Right.
-- Otherwise, returns the first Left error message.
allRight :: [EvalResult] -> Either String [EvalValue]
allRight = foldr (liftA2 (:)) (Right [])


-- Returns either an error string or a resulting integer.
eval :: Expr -> EvalResult
eval = evalExpr []

-- Bindings are the variables in scope.
--
-- Returns either an error string or a resulting number.
evalExpr :: Bindings -> Expr -> EvalResult
evalExpr bindings expr = case expr of
  Number n    -> Right (NumberValue n)
  Var    name -> case lookup name bindings of
    Just value -> Right value
    Nothing    -> Left $ "could not find variable \"" ++ name ++ "\""
  Boolean b            -> Right (BoolValue b)
  Not     e            -> unOpB not e "cannot not a number"
  Comparison cmp e1 e2 -> case (recurse e1, recurse e2) of
    (Right (BoolValue b1), Right (BoolValue b2)) ->
      let op = cmpTable M.! cmp in Right . BoolValue $ op b1 b2
    (Right (NumberValue n1), Right (NumberValue n2)) ->
      let op = cmpTable M.! cmp in Right . BoolValue $ op n1 n2
    (Right _, Right _) -> Left "cannot compare booleans and numbers"
    (Right _, err    ) -> err
    (err    , Right _) -> err
    (err1   , err2   ) -> err1 <> err2
  Neg e       -> unOpN negate e "cannot negate boolean"
  Plus  e1 e2 -> binOpN (+) e1 e2 "plus"
  Minus e1 e2 -> binOpN subtract e2 e1 "minus"
  Mult  e1 e2 -> binOpN (*) e1 e2 "times"
  Div   e1 e2 -> if recurse e2 == Right (NumberValue 0)
    then Left "division by zero"
    else binOpN quot e1 e2 "divide"
  Pow e1 e2 -> case recurse e2 of
    Right (NumberValue pow) ->
      if pow < 0 then Left "negative exponent" else binOpN (^) e1 e2 "power"
    Right _   -> Left "raised to a boolean power"
    Left  err -> Left err
  Let names mathExps mainExp | length names == length mathExps -> do
    newbindings <- zip names <$> (allRight . map (evalExpr bindings) $ mathExps)
    evalExpr (bindings ++ newbindings) mainExp
  Let names mathExps mainExp ->
    Left
      $  "must assign "
      ++ show (length names)
      ++ " names but given "
      ++ show (length mathExps)
      ++ " expressions"
  If condition ifTrue ifFalse -> case recurse condition of
    Right (BoolValue b) -> if b then recurse ifTrue else recurse ifFalse
    Right _             -> Left "number value in a condition context"
    Left  err           -> Left err
 where
  recurse = evalExpr bindings
  unOpN op e errMsg = case recurse e of
    Right (NumberValue n) -> Right . NumberValue $ op n
    Right (BoolValue   _) -> Left errMsg
    left                  -> left
  unOpB op e errMsg = case recurse e of
    Right (BoolValue   b) -> Right . BoolValue $ op b
    Right (NumberValue _) -> Left errMsg
    left                  -> left

  binOpN op e1 e2 name = case (recurse e1, recurse e2) of
    (Right (NumberValue n1), Right (NumberValue n2)) ->
      Right . NumberValue $ op n1 n2
    (Right (BoolValue _), _                  ) -> Left errMsg
    (_                  , Right (BoolValue _)) -> Left errMsg
    (Right _            , Left err           ) -> Left err
    (Left  err          , Right _            ) -> Left err
    (left1              , left2              ) -> left1 <> left2
   where
    errMsg = "cannot perform operation `" ++ name ++ "` in a boolean context"
