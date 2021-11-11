module Eval (
  EvalResult,
  eval
) where

import           Control.Applicative
import           Lab6

-- Error message or result number.
type EvalResult = Either String Number

type Bindings = [(Name, Number)]


-- Return wrapped list of bare results if all inputs are Right.
-- Otherwise, returns the first Left error message.
allRight :: [EvalResult] -> Either String [Number]
allRight = foldr (liftA2 (:)) (Right [])


-- Returns either an error string or a resulting integer.
eval :: TopLevelExp -> EvalResult
eval (MathTLE mathExp)              = evalMathExp [] mathExp
eval (LetTLE names mathExps mainExp)
    | length names == length mathExps = performEval
    | otherwise                       = Left errorMsg
    where
        performEval = do
            bindings <- bindingsOrError
            evalMathExp bindings mainExp
        bindingsOrError =
            zip names <$> (allRight . map (evalMathExp []) $ mathExps)
        errorMsg =
            "must assign " ++ show (length names) ++
            " names but given " ++ show (length mathExps) ++
            " expressions"

-- Bindings are the variables in scope.
--
-- Returns either an error string or a resulting number.
evalMathExp :: Bindings -> MathExp -> EvalResult
evalMathExp bindings exp =
    let
        recurse        = evalMathExp bindings
        unOp op e      = op <$> recurse e
        binOp op e1 e2 = liftA2 op (recurse e1) (recurse e2)
    in
    case exp of
        Number n -> Right n
        Var name ->
            case lookup name bindings of
                Just n  -> Right n
                Nothing -> Left $ "could not find variable \"" ++ name ++ "\""
        Neg   e     -> unOp negate e
        Plus  e1 e2 -> binOp (+)      e1 e2
        Minus e1 e2 -> binOp subtract e2 e1
        Mult  e1 e2 -> binOp (*)      e1 e2
        Div   e1 e2 ->
            if recurse e2 == Right 0
            then Left "division by zero"
            else binOp quot e1 e2
        Pow   e1 e2 ->
            case recurse e2 of
                Right pow -> if pow < 0
                             then Left "negative exponent"
                             else binOp (^) e1 e2
                Left err  -> Left err
