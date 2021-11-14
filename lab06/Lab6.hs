-- Write your parser in this file.

module Lab6
  ( Name
  , Number
  , cmpTable
  , Expr(..)
  , parse
  ) where

import           Control.Applicative            ( liftA2 )
import           Data.Char
import           Data.List
import qualified Data.Map.Strict               as M
import           Data.Ord
import           Text.ParserCombinators.ReadP

cmpTable :: Ord a => M.Map String (a -> a -> Bool)
cmpTable = M.fromList
  [ ("<" , (<))
  , ("<=", (<=))
  , (">" , (>))
  , (">=", (>=))
  , ("==", (==))
  , ("/=", (/=))
  ]

spaceableParser :: ReadP a -> ReadP a
spaceableParser = between skipSpaces skipSpaces

spaceableChar :: Char -> ReadP Char
spaceableChar = spaceableParser . char

type Name = String  -- Variable names are strings.
type Number = Int     -- The kind of number in our language.

data Expr
    = Number     Number
    | Var        Name
    | Boolean    Bool
    | Not        Expr
    | Comparison String Expr Expr
    | Neg        Expr
    | Plus       Expr Expr
    | Minus      Expr Expr
    | Mult       Expr Expr
    | Div        Expr Expr
    | Pow        Expr Expr
    | Let        [Name] [Expr] Expr
    | If         Expr Expr Expr
    deriving (Eq, Show)

parsePossiblyNegExpr :: ReadP Expr -> ReadP Expr
parsePossiblyNegExpr p = (Neg <$> (spaceableChar '-' *> p)) +++ p

parseNumber :: ReadP Expr
parseNumber = Number . read <$> many1 (satisfy isDigit)

parsePossiblyNegNumber :: ReadP Expr
parsePossiblyNegNumber = parsePossiblyNegExpr parseNumber

parseName :: ReadP [Char]
parseName = (:) <$> satisfy isLower <*> many (satisfy isAlphaNum)

parseVarName :: ReadP Expr
parseVarName = Var <$> do
  name <- parseName
  if name `elem` ["let", "in", "if", "not", "else", "then"]
    then pfail
    else return name

parseBoolValue :: ReadP Expr
parseBoolValue =
  (Boolean True <$ string "True") +++ (Boolean False <$ string "False")

parsePossiblyNegVarName :: ReadP Expr
parsePossiblyNegVarName = parsePossiblyNegExpr parseVarName

parseCondition :: ReadP Expr
parseCondition = do
  pTrue <- spaceableParser $ option "" (string "not ")
  cmp   <- parseExpr
  if null pTrue then return cmp else return $ Not cmp
  where ops = M.keys (cmpTable :: M.Map String (Number -> Number -> Bool))

parseOps :: [String] -> ReadP (Expr -> Expr -> Expr)
parseOps op = do
  str <- spaceableParser . choice $ map string op
  return $ opTable M.! str
 where
  opTable = M.fromList
    [ ("+" , Plus)
    , ("-" , Minus)
    , ("*" , Mult)
    , ("/" , Div)
    , ("^" , Pow)
    , ("<" , Comparison "<")
    , ("<=", Comparison "<=")
    , (">" , Comparison ">")
    , (">=", Comparison ">=")
    , ("==", Comparison "==")
    , ("/=", Comparison "/=")
    ]

parsePlusAndMinus, parseMultAndDiv, parsePow, parseCmp
  :: ReadP (Expr -> Expr -> Expr)
parsePlusAndMinus = parseOps ["+", "-"]
parseMultAndDiv = parseOps ["*", "/"]
parsePow = parseOps ["^"]
parseCmp = parseOps ["<", "<=", ">", ">=", "==", "/="]

parseFlat :: ReadP Expr
parseFlat = spaceableParser $ parseNumber +++ parseVarName +++ parseBoolValue

parseInParens :: ReadP a -> ReadP a
parseInParens = between (spaceableChar '(') (spaceableChar ')')

parseParenExp :: ReadP Expr
parseParenExp = parseInParens parseExpr

parsePossiblyNegParen :: ReadP Expr
parsePossiblyNegParen = parsePossiblyNegExpr parseParenExp

parseTuple :: ReadP a -> ReadP [a]
parseTuple p = parseInParens $ p `sepBy` spaceableChar ','

parseNameTuple :: ReadP [Name]
parseNameTuple = parseTuple parseName

parseTupleExpr :: ReadP [Expr]
parseTupleExpr = parseTuple parseExpr

filterUnfinishedExpr :: ReadP Expr
filterUnfinishedExpr = do
  exp  <- parseExpr
  rest <- look
  case rest of
    (x : _) | x `elem` ['+', '-', '/', '*', '^'] -> pfail
    _ -> return exp

parseLet :: ReadP Expr
parseLet = do
  spaceableParser $ string "let"
  names <- parseNameTuple <++ fmap (: []) parseName
  spaceableChar '='
  values <- parseTupleExpr <++ fmap (: []) parseExpr
  spaceableParser $ string "in"
  Let names values <$> filterUnfinishedExpr

parseIf :: ReadP Expr
parseIf = do
  string "if"
  condition <- parseCondition
  string "then"
  ifTrue <- parseExpr
  string "else"
  If condition ifTrue <$> filterUnfinishedExpr

parseExpr :: ReadP Expr
parseExpr = chainl1 parseBase parseCmp
 where
  parseBase   = chainl1 parseHigher parsePlusAndMinus
  parseHigher = chainl1 parseEvenHigher parseMultAndDiv
  parseEvenHigher = -- I know I took the joke literally but they really are good names
    parsePossiblyNegExpr $ chainr1 parseEvenMoreHigher parsePow
  parseEvenMoreHigher = parseIf <++ parseLet <++ parseParenExp <++ parseFlat


-- Run the parser on a given string.
--
-- You should not modif y this function. Grading may
-- look for the specific messages below.
parse :: String -> Either String Expr
parse str = case (completeParses, incompleteParses) of
  ([(result, "")], _) -> Right result  -- Only complete result.
  ([], []) -> Left "No parse."
  ([], _ : _) -> Left $ "Incomplete parse. Unparsed: " ++ show leastRemaining
  (_ : _, _) -> Left $ "Ambiguous parse: " ++ show (map fst completeParses)
 where
  parses = readP_to_S parseExpr str
  (completeParses, incompleteParses) =
    partition (\(_, remaining) -> remaining == "") parses
  leastRemaining = minimumBy (comparing length) . map snd $ incompleteParses
