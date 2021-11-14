-- Unparse expressions for debug display.
--
-- You should not need to modify this for the regular lab.
--
module Unparse
  ( unparse
  ) where

import           Data.List
import           Lab6


unparse :: MathExp -> String
unparse (Number n   ) = show n
unparse (Var    name) = name
unparse (Neg    e1  ) = "-" ++ unparse e1
unparse (Plus  e1 e2) = "(" ++ unparse e1 ++ " + " ++ unparse e2 ++ ")"
unparse (Minus e1 e2) = "(" ++ unparse e1 ++ " - " ++ unparse e2 ++ ")"
unparse (Mult  e1 e2) = "(" ++ unparse e1 ++ " * " ++ unparse e2 ++ ")"
unparse (Div   e1 e2) = "(" ++ unparse e1 ++ " / " ++ unparse e2 ++ ")"
unparse (Pow   e1 e2) = "(" ++ unparse e1 ++ "^" ++ unparse e2 ++ ")"
unparse (Let names assigns main) =
  "(let "
    ++ commaList names
    ++ " = "
    ++ (commaList . map unparse $ assigns)
    ++ " in "
    ++ unparse main
    ++ ")"
 where
  commaList [str] = str
  commaList strs  = "(" ++ intercalate ", " strs ++ ")"


