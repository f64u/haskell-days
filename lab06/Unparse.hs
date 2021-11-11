-- Unparse expressions for debug display.
--
-- You should not need to modify this for the regular lab.
--
module Unparse (
  unparse
) where

import           Data.List
import           Lab6

unparse :: TopLevelExp -> String
unparse (MathTLE mathExp)           = unparseMathExp mathExp
unparse (LetTLE names assigns main) =
    "let " ++ commaList names ++ " = " ++
    (commaList . map unparseMathExp $ assigns) ++ " in " ++
    unparseMathExp main
    where
        commaList [str] =  str
        commaList strs  = "(" ++ intercalate ", " strs ++ ")"

unparseMathExp :: MathExp -> String
unparseMathExp (Number n)     = show n
unparseMathExp (Var    name)  = name
unparseMathExp (Neg    e1)    = "-" ++ unparseMathExp e1
unparseMathExp (Plus   e1 e2) = "(" ++ unparseMathExp e1 ++ " + " ++ unparseMathExp e2 ++ ")"
unparseMathExp (Minus  e1 e2) = "(" ++ unparseMathExp e1 ++ " - " ++ unparseMathExp e2 ++ ")"
unparseMathExp (Mult   e1 e2) = "(" ++ unparseMathExp e1 ++ " * " ++ unparseMathExp e2 ++ ")"
unparseMathExp (Div    e1 e2) = "(" ++ unparseMathExp e1 ++ " / " ++ unparseMathExp e2 ++ ")"
unparseMathExp (Pow    e1 e2) = "(" ++ unparseMathExp e1 ++  "^"  ++ unparseMathExp e2 ++ ")"


