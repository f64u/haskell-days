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
import           Debug.Trace
import           Text.ParserCombinators.ReadP

-- | Relates a string representing a comparison to its haskell-function equivalent 
cmpTable :: Ord a => M.Map String (a -> a -> Bool)
cmpTable = M.fromList
  [ ("<" , (<))
  , ("<=", (<=))
  , (">" , (>))
  , (">=", (>=))
  , ("==", (==))
  , ("/=", (/=))
  ]

keywords :: [String]
keywords = ["let", "in", "if", "else", "then"]

-- | Allows a parser to be surrounded by spaces from either sides
spaceableParser :: ReadP a -> ReadP a
spaceableParser = between skipSpaces skipSpaces

-- | Allows a character to be surrounded by spaces from either sides
spaceableChar :: Char -> ReadP Char
spaceableChar = spaceableParser . char

-- | Allows a string to be surrounded by spaces from either sides
spaceableString :: String -> ReadP String
spaceableString = spaceableParser . string

type Name = String  -- Variable names are strings.
type Number = Int     -- The kind of number in our language.

-- | Holding all expression structures that could be present at top value or nested with other expressions 
--   according to "rules"
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
    | Lambda     Name Expr
    | Apply      Expr Expr
    deriving (Eq, Show)

-- | Allows a parser to be prefixed by a prefix and transforming the resulting expr into
--   its corresponding prefixed counterpart
possiblyPrefixedExpr :: (Expr -> Expr) -> ReadP a -> ReadP Expr -> ReadP Expr
possiblyPrefixedExpr typer prefix parser =
  (typer <$> (prefix *> skipSpaces *> parser)) +++ parser

-- | Allows a parser to be prefixed by a negative sign (-)
parsePossiblyNegExpr :: ReadP Expr -> ReadP Expr
parsePossiblyNegExpr = possiblyPrefixedExpr Neg (char '-')

-- | Parses a number
parseNumber :: ReadP Expr
parseNumber = Number . read <$> many1 (satisfy isDigit)

-- | Parses a name adhering to variable naming rules
parseName :: ReadP String
parseName = (:) <$> satisfy isLower <*> many (satisfy isAlphaNum)

-- | Parses a variable that is not a reserved keyword
parseVarName :: ReadP Expr
parseVarName = Var <$> do
  name <- parseName
  if name `elem` keywords then pfail else return name

-- | Parses the bool values True and False
parseBoolValue :: ReadP Expr
parseBoolValue =
  (Boolean True <$ string "True") +++ (Boolean False <$ string "False")

-- | Parses a binary operator, returns a parser that knows how to combine the exprs on both sides into
--   one expr
parseOps :: [ReadP String] -> ReadP (Expr -> Expr -> Expr)
parseOps op = do
  str <- choice op
  return $ opTable M.! str
 where
  opTable = M.fromList
    [ ("+" , Plus)
    , ("-" , Minus)
    , ("*" , Mult)
    , ("/" , Div)
    , ("^" , Pow)
    , (" " , Apply)
    -- I know this isn't exactly the haskell-way of interpreting comparisons, but I think it is intuitive enough and I like it
    -- For example, this allows `3 == 4 == False` which evaluates to True
    , ("<" , Comparison "<")
    , ("<=", Comparison "<=")
    , (">" , Comparison ">")
    , (">=", Comparison ">=")
    , ("==", Comparison "==")
    , ("/=", Comparison "/=")
    ]

-- | Each group operates on a specific operator precedency, increasing 
parseCmp, parsePlusAndMinus, parseMultAndDiv, parsePow, parseAp
  :: ReadP (Expr -> Expr -> Expr)
parseCmp = parseOps $ map spaceableString ["<", "<=", ">", ">=", "==", "/="]
parsePlusAndMinus = parseOps $ map spaceableString ["+", "-"]
parseMultAndDiv = parseOps $ map spaceableString ["*", "/"]
parsePow = parseOps $ map spaceableString ["^"]
parseAp = parseOps [" " <$ many1 (char ' ')]

-- | Parses a "flat" value, either a number, a variable, or a boolean
parseFlat :: ReadP Expr
parseFlat = parseNumber +++ parseVarName +++ parseBoolValue

-- | Parses a generic parser in parentheses 
parseInParens :: ReadP a -> ReadP a
parseInParens = between (char '(' <* skipSpaces) (skipSpaces *> char ')')

-- | Parses an expr in parentheses 
parseParenExp :: ReadP Expr
parseParenExp = parseInParens parseExpr

-- | Parses a tuple of a generic parser
parseTuple :: ReadP a -> ReadP [a]
parseTuple p = parseInParens $ p `sepBy` (skipSpaces *> char ',' <* skipSpaces)

-- | Parses a tuple of names 
parseNameTuple :: ReadP [Name]
parseNameTuple = parseTuple parseName

-- | Parses a tuple of exprs
parseTupleExpr :: ReadP [Expr]
parseTupleExpr = parseTuple parseExpr

-- | Returns a parse if and only if it couldn't parse more expr out of string.
--   The way it does this is that it looks if there is more operators unparsed. 
--   Helpful in enforcing the presedence of let and if. Feels hacky ngl.
filterUnfinishedExpr :: ReadP Expr
filterUnfinishedExpr = do
  exp  <- parseExpr
  rest <- look
  if any (`isPrefixOf` rest)
         ["+", "-", "*", "/", "^", "<", "<=", ">", ">=", "==", "/=", " "]
       && not
            (any (`isPrefixOf` dropWhile isSpace rest)
                 ["let", "in", "if", "not", "else", "then"]
            ) -- this is specifically for application: do not apply on keywords even if there's space
    then pfail
    else return exp

-- | Parses a let expr
parseLet :: ReadP Expr
parseLet = do
  spaceableString "let"
  names <- parseNameTuple <++ fmap (: []) parseName
  spaceableChar '='
  values <- parseTupleExpr <++ fmap (: []) parseExpr
  spaceableString "in"
  Let names values <$> filterUnfinishedExpr

-- | Parses an if expr
parseIf :: ReadP Expr
parseIf = do
  spaceableString "if"
  condition <- filterUnfinishedExpr
  spaceableString "then"
  ifTrue <- parseExpr
  spaceableString "else"
  If condition ifTrue <$> filterUnfinishedExpr

-- | Parses a lambda expr
parseLambda :: ReadP Expr
parseLambda = parseInParens $ do
  spaceableChar '\\'
  name <- parseName
  spaceableString "->"
  Lambda name <$> parseExpr

-- | Parses an expr
parseExpr :: ReadP Expr
parseExpr = skipSpaces *> chainl1 parseBase parseCmp
 where
  parseBase           = chainl1 parseHigher parsePlusAndMinus  -- yes this is a later addition how did you know
  parseHigher         = chainl1 parseEvenHigher parseMultAndDiv -- I know I took the joke literally but they really are good names
  parseEvenHigher = parsePossiblyNegExpr $ chainr1 parseEvenMoreHigher parsePow
  parseEvenMoreHigher = chainl1 parseEvenMoreMoreHigher parseAp
  parseEvenMoreMoreHigher =
    parseIf <++ parseLet <++ parseLambda <++ parseParenExp <++ parseFlat


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
