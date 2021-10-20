module Lab3 where
import           Data.Char

import           Data.List
import           Data.Maybe
import           Data.Ratio

data ArithExp = Number Rational | Op Char ArithExp ArithExp deriving (Show, Eq)

eval :: ArithExp -> Rational
eval (Number n ) = n
eval (Op op l r) = fromJust (lookup op funcLookup) (eval l) (eval r)
  where funcLookup = [('+', (+)), ('*', (*)), ('/', (/))]

data Token = TInt Integer | TNeg | TPlus | TMult | TDiv | TParen [Token] deriving (Show, Eq)

tokenize :: String -> [Token]
tokenize = fst . tokenizeTillClose . filter (not . isSpace)

tokenizeTillClose :: String -> ([Token], String)
tokenizeTillClose ""           = ([], "")
tokenizeTillClose (')' : rest) = ([], rest)
tokenizeTillClose ('(' : rest) =
  let (tokens, otherRest) = tokenizeTillClose rest
  in  prependTok (TParen tokens) (tokenizeTillClose otherRest)
tokenizeTillClose (char : rest) = prependTok (tokLookup char)
  $ tokenizeTillClose rest
 where
  tokLookup c | isDigit c = TInt (fromIntegral $ digitToInt c)
              | otherwise = fromJust $ lookup c table
  table = [('-', TNeg), ('+', TPlus), ('*', TMult), ('/', TDiv)]

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
                  then Op '*' (parse $ init l_multdiv) (parse r_multdiv)
                  else Op '/' (parse $ init l_multdiv) (parse r_multdiv)
        else Op '+' (parse l_plus) (parse $ tail r_plus)
 where
  parseOpless (TParen tokens : _) = parse tokens -- TParen can only be surrounded with ops, so if we got here, nothing is beside it
  parseOpless (TNeg : ns) = let (Number n) = parseOpless ns in Number (-n)
  parseOpless ns = Number . sum $ zipWith (\i (TInt n) -> 10 ^ i * (n % 1))
                                          (reverse [0 .. length ns - 1])
                                          ns
