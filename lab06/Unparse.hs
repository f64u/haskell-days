-- Unparse expressions for debug display.
--
-- You should not need to modify this for the regular lab.
--
module Unparse
  ( unparse
  , commaList
  ) where

import           Data.List
import           Lab6

commaList :: [String] -> String
commaList [str] = str
commaList strs  = "(" ++ intercalate ", " strs ++ ")"


unparse :: Expr -> String
unparse (Number  n           ) = show n
unparse (Var     name        ) = name
unparse (Boolean x           ) = show x
unparse (Comparison cmp e1 e2) = unparse e1 ++ " " ++ cmp ++ " " ++ unparse e2
unparse (Not e1              ) = "not " ++ unparse e1
unparse (Neg e1              ) = "(-" ++ unparse e1 ++ ")"
unparse (Plus  e1 e2         ) = "(" ++ unparse e1 ++ " + " ++ unparse e2 ++ ")"
unparse (Minus e1 e2         ) = "(" ++ unparse e1 ++ " - " ++ unparse e2 ++ ")"
unparse (Mult  e1 e2         ) = "(" ++ unparse e1 ++ " * " ++ unparse e2 ++ ")"
unparse (Div   e1 e2         ) = "(" ++ unparse e1 ++ " / " ++ unparse e2 ++ ")"
unparse (Pow   e1 e2         ) = "(" ++ unparse e1 ++ "^" ++ unparse e2 ++ ")"
unparse (Let names assigns main) =
  "(let "
    ++ commaList names
    ++ " = "
    ++ (commaList . map unparse $ assigns)
    ++ " in "
    ++ unparse main
    ++ ")"
unparse (If condition ifTrue ifFalse) =
  "(if "
    ++ unparse condition
    ++ " then "
    ++ unparse ifTrue
    ++ " else "
    ++ unparse ifFalse
    ++ ")"
unparse (Lambda name expr) = "(\\" ++ name ++ " -> " ++ unparse expr ++ ")"
unparse (Apply expr1 expr2) =
  "(" ++ unparse expr1 ++ " " ++ unparse expr2 ++ ")"
