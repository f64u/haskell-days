module Lab3 where

import           Data.Char
import           Data.List
import           Data.Maybe
import           Debug.Trace

data ArithExp = Number Int | Plus ArithExp ArithExp | Mult ArithExp ArithExp | Div ArithExp ArithExp deriving (Show, Eq)

eval :: ArithExp -> Int
eval (Number n      ) = n
eval (Plus exp1 exp2) = eval exp1 + eval exp2
eval (Mult exp1 exp2) = eval exp1 * eval exp2
eval (Div  exp1 exp2) = eval exp1 `quot` eval exp2

data Token = TInt Int | TPlus | TNeg | TMult | TDiv | TParen [Token] deriving (Show, Eq)

-- Lookup table for our tokinzer
lookup' :: Char -> Token
lookup' c | isDigit c = TInt (digitToInt c)
          | c == '+'  = TPlus
          | c == '-'  = TNeg
          | c == '*'  = TMult
          | c == '/'  = TDiv
          | otherwise = error "This shouldn't happen"

tokenize :: String -> [Token]
tokenize = fst . tokenizeTillClose

tokenizeTillClose :: String -> ([Token], String)
tokenizeTillClose "" = ([], "")
tokenizeTillClose s =
  let (prelparen, lparenon) = break (== '(') s
  in  if null lparenon
           || (case (')' `elemIndex` s, '(' `elemIndex` s) of
                (Just r, Just l) -> l > r
                _                -> False
              )
        then
          let (prerparen, rparenon) = break (== ')') s
          in  (tokenizeParenless prerparen, tail rparenon)
        else prependToks
          (tokenizeParenless prelparen)
          (let (tokens, rest) = tokenizeTillClose (tail lparenon)
           in  prependToks [TParen tokens] (tokenizeTillClose rest)
          )
 where
  tokenizeParenless = map lookup' . filter (not . isSpace)
  prependToks toks1 (toks2, rest) = (toks1 ++ toks2, rest)

parse :: [Token] -> ArithExp
parse tokens =
  let (l_plus, r_plus) = break (== TPlus) tokens
  in  if null r_plus
        then
          let (r_multdiv', l_multdiv') =
                break (\x -> x == TMult || x == TDiv) (reverse tokens)
              (r_multdiv, l_multdiv) = (reverse r_multdiv', reverse l_multdiv')
          in  if null l_multdiv
                then parseOpless tokens
                else if last l_multdiv == TMult
                  then Mult (parse $ init l_multdiv) (parse r_multdiv)
                  else Div (parse $ init l_multdiv) (parse r_multdiv)
        else Plus (parse l_plus) (parse $ tail r_plus)
 where
  parseOpless (TParen tokens : _) = parse tokens
  parseOpless (TNeg : ns) = let (Number n) = parseOpless ns in Number (-n)
  parseOpless ns = Number . sum $ zipWith (\i (TInt n) -> 10 ^ i * n)
                                          (reverse [0 .. length ns - 1])
                                          ns

