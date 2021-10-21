module Lab3 where

import           Data.Char
import           Data.Maybe
import           Data.Ratio

data ArithExp = Number Rational | Op Char ArithExp ArithExp deriving (Show, Eq)

data Token = TInt Integer | TOp Char | TParen [Token] deriving (Show, Eq)

-- offers extensiblity in terms of potential new binary functions (only to eval tho)
charDict = [('+', (+)), ('*', (*)), ('/', (/))]

eval :: ArithExp -> Rational
eval (Number n ) = n
eval (Op op l r) = fromJust (lookup op charDict) (eval l) (eval r)

tokenize :: String -> [Token]
tokenize = fst . tokenizeTillClose . filter (not . isSpace)
 where
  tokenizeTillClose ""           = ([], "")
  tokenizeTillClose (')' : rest) = ([], rest)
  tokenizeTillClose ('(' : rest) =
    let (toks, rest') = tokenizeTillClose rest
    in  prependTok (TParen toks) (tokenizeTillClose rest')
  tokenizeTillClose (char : rest) =
    prependTok
        (if isDigit char
          then TInt . fromIntegral $ digitToInt char
          else TOp char
        )
      $ tokenizeTillClose rest
  prependTok tok (toks, rest) = (tok : toks, rest)

parse :: [Token] -> ArithExp
parse toks = case (null rplus, null lmultdiv) of -- check if
  (True, True) -> parseOpless toks -- no pluses or timeses or divides available; if not proceed op-less
  (True, _   ) -> Op (let (TOp char) = last lmultdiv in char) -- no pluses available; if not proceed factorily
                     (parse $ init lmultdiv)
                     (parse rmultdiv) 
  _ -> Op '+' (parse lplus) (parse $ tail rplus) -- only pluses available, in that order; if so do that
 where
  (lplus   , rplus   ) = break (== TOp '+') toks -- Don't have to split backwards, plus is commutative with plus
  (rmultdiv, lmultdiv) = (\f (a, b) -> (f a, f b)) reverse -- sucks to have to split backwards (this lambda should've been library-provided IMO)
    $ break (`elem` [TOp '*', TOp '/']) (reverse toks) 
  parseOpless (TParen toks : _) = parse toks
  parseOpless (TOp '-' : toks) =
    let (Number n) = parseOpless toks in Number (-n)
  parseOpless toks = Number . sum $ zipWith (\i (TInt n) -> 10 ^ i * (n % 1))
                                            (reverse [0 .. length toks - 1])
                                            toks
