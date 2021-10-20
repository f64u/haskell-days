module Lab3 where

import           Data.Char
import           Data.List
import           Data.Ratio

data ArithExp = Number Rational | Plus ArithExp ArithExp | Mult ArithExp ArithExp | Div ArithExp ArithExp deriving (Show, Eq)

eval :: ArithExp -> Rational
eval (Number n) = n
eval (Plus l r) = eval l + eval r
eval (Mult l r) = eval l * eval r
eval (Div  l r) = eval l / eval r

data Token = TInt Integer | TNeg | TPlus | TMult | TDiv | TParen [Token] deriving (Show, Eq)

-- Lookup table for our tokinzer
lookup' :: Char -> Token
lookup' c | isDigit c = TInt (fromIntegral $ digitToInt c)
          | c == '-'  = TNeg
          | c == '+'  = TPlus
          | c == '*'  = TMult
          | c == '/'  = TDiv
          | otherwise = error $ "This shouldn't happen: got " ++ show c

tokenize :: String -> [Token]
tokenize = fst . tokenizeTillClose . filter (not . isSpace)

tokenizeTillClose :: String -> ([Token], String)
tokenizeTillClose ""           = ([], "")
tokenizeTillClose (')' : rest) = ([], rest)
tokenizeTillClose ('(' : rest) =
  let (tokens, otherRest) = tokenizeTillClose rest
  in  prependTok (TParen tokens) (tokenizeTillClose otherRest)
tokenizeTillClose (char : rest) =
  prependTok (lookup' char) $ tokenizeTillClose rest

prependTok :: a -> ([a], b) -> ([a], b)
prependTok tok (toks, rest) = (tok : toks, rest)

parse :: [Token] -> ArithExp
parse tokens =
  let (l_plus, r_plus) = break (== TPlus) tokens -- thank god + is commutative
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
  parseOpless (TParen tokens : _) = parse tokens -- TParen can only be surrounded with ops, so if we got here, nothing is beside it
  parseOpless (TNeg : ns) = let (Number n) = parseOpless ns in Number (-n)
  parseOpless ns = Number . sum $ zipWith (\i (TInt n) -> 10 ^ i * (n % 1))
                                          (reverse [0 .. length ns - 1])
                                          ns
