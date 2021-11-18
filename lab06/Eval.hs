module Eval
  ( EvalResult
  , evalExpr
  ) where

import qualified Data.Map.Strict               as M

import           Control.Monad.State
import           Lab6
import           Unparse                        ( commaList
                                                , unparse
                                                )

-- | Possible types in our evaluator
data EvalValue = NumberValue Number | BoolValue Bool | LambdaValue Bindings Name Expr deriving (Eq)
instance Show EvalValue where
  show (NumberValue n) = show n
  show (BoolValue   b) = show b
  show (LambdaValue bindings name expr) =
    "let "
      ++ commaList (map fst bindings)
      ++ " = "
      ++ commaList (map (show . snd) bindings)
      ++ " in "
      ++ unparse (Lambda name expr)

-- | Error message or result number.
type EvalResult = Either String EvalValue

type Bindings = [(Name, EvalValue)]

-- | Error-aware assignment context
type Context = State Bindings (Either String Bindings)

-- Bindings are the variables in scope.
--
-- Returns either an error string or a resulting number.
-- I really like how flexible this turned out. Booleans and Numbers are handeled similarly until you put them in context,
-- which then can result in an error (for example if you put a number in the condition part of the if expression). But aside from that,
-- Booleans can be variables, can be operated on by keywords, and have binary operations associated with them, much like numbers.
evalExpr :: Bindings -> Expr -> EvalResult
evalExpr bindings expr = case expr of
  Number n    -> Right (NumberValue n)
  Var    name -> case lookup name bindings of
    Just value -> Right value
    Nothing    -> Left $ "could not find variable \"" ++ name ++ "\""
  Boolean b            -> Right (BoolValue b)
  Not     e            -> unOpB not e "can only not a boolean"
  Comparison cmp e1 e2 -> compareExprs cmp e1 e2
  Neg e                -> unOpN negate e "can only negate numbers"
  Plus  e1 e2          -> binOpN (+) e1 e2 "plus"
  Minus e1 e2          -> binOpN subtract e2 e1 "minus"
  Mult  e1 e2          -> binOpN (*) e1 e2 "times"
  Div   e1 e2          -> if recurse e2 == Right (NumberValue 0)
    then Left "division by zero"
    else binOpN quot e1 e2 "divide"
  Pow e1 e2 -> case recurse e2 of
    Right (NumberValue pow) ->
      if pow < 0 then Left "negative exponent" else binOpN (^) e1 e2 "power"
    Right _ -> Left "raised to a non-numerical exponent"
    err     -> err
  Let names valueExprs expr | length names == length valueExprs ->
    let result = evalState (prependBindings (zip names valueExprs)) bindings
    in  case result of
          Right newBindings -> evalExpr newBindings expr
          Left  err         -> Left err
   where
    -- Oooh immutable haskell how much I love you; this allows previous assignments to get
    -- accounted for in the new assignments in the same let expression. 
    -- > let (x, y) = (6, x*7) in y 
    -- 42
    -- This may be an overcomplication of stuff idk.
    addBinding :: (Name, Expr) -> Context
    addBinding (name, expr) = do
      local <- get
      case evalExpr local expr of
        Right value -> do
          let x = (name, value) : local
          put x
          return . Right $ x
        Left err -> return $ Left err
    prependBindings :: [(Name, Expr)] -> Context
    prependBindings []       = state $ \s -> (Right s, s)
    prependBindings [b     ] = addBinding b
    prependBindings (b : bs) = do
      result <- addBinding b
      case result of
        Right _ -> do -- I don't really need its value
          newResult <- prependBindings bs
          case newResult of
            Right actualBindings -> return . Right $ actualBindings
            Left  err            -> return $ Left err
        Left err -> return $ Left err

  Let names mathExps _ ->
    Left
      $  "must assign "
      ++ show (length names)
      ++ " names but given "
      ++ show (length mathExps)
      ++ " expressions"
  If condition ifTrue ifFalse -> case recurse condition of
    Right (BoolValue b) -> if b then recurse ifTrue else recurse ifFalse
    Right _             -> Left "non-boolean value in a condition context"
    err                 -> err
  Lambda name  expr  -> Right $ LambdaValue bindings name expr
  Apply  expr1 expr2 -> case evalExpr bindings expr1 of
    Right (LambdaValue closureBindings name expr) ->
      case evalExpr bindings expr2 of
        Right value ->
          evalExpr (closureBindings ++ bindings ++ [(name, value)]) expr
        err -> err
    Right _ -> Left "Trying to apply a non-lambda value"
    err     -> err
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
    (Right _            , err                ) -> err
    (err                , Right _            ) -> err
    (err1               , err2               ) -> err1 <> err2
   where
    errMsg =
      "cannot perform operation `" ++ name ++ "` in a non-numerical context"

  compareExprs cmp e1 e2 = case (recurse e1, recurse e2) of
    (Right (BoolValue b1), Right (BoolValue b2)) ->
      let op = cmpTable M.! cmp in Right . BoolValue $ op b1 b2
    (Right (NumberValue n1), Right (NumberValue n2)) ->
      let op = cmpTable M.! cmp in Right . BoolValue $ op n1 n2
    (Right _, Right _) -> Left "cannot compare different types"
    (Right _, err    ) -> err
    (err    , Right _) -> err
    (err1   , err2   ) -> err1 <> err2
