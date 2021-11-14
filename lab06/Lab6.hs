-- Write your parser in this file.

module Lab6
  ( Name
  , Number
  , MathExp(..)
  , parse
  ) where

import           Control.Applicative            ( liftA2 )
import           Data.Char
import           Data.List
import qualified Data.Map.Strict               as M
import           Data.Ord
import           Text.ParserCombinators.ReadP

type Name = String  -- Variable names are strings.
type Number = Int     -- The kind of number in our language.

data MathExp
    = Number Number
    | Var    Name
    | Neg    MathExp
    | Plus   MathExp MathExp
    | Minus  MathExp MathExp
    | Mult   MathExp MathExp
    | Div    MathExp MathExp
    | Pow    MathExp MathExp
    | Let [Name] [MathExp] MathExp
    deriving (Eq, Show)

parsePossiblyNegExp :: ReadP MathExp -> ReadP MathExp
parsePossiblyNegExp p = (Neg <$> (char '-' *> p)) +++ p

parseNumber :: ReadP MathExp
parseNumber = Number . read <$> many1 (satisfy isDigit)

parsePossiblyNegNumber :: ReadP MathExp
parsePossiblyNegNumber = parsePossiblyNegExp parseNumber

parseName :: ReadP [Char]
parseName = (:) <$> satisfy isLower <*> many (satisfy isAlphaNum)

parseVarName :: ReadP MathExp
parseVarName = Var <$> do
  name <- parseName
  if name `elem` ["let", "in"] then pfail else return name

parsePossiblyNegVarName :: ReadP MathExp
parsePossiblyNegVarName = parsePossiblyNegExp parseVarName

parseOps :: [Char] -> ReadP (MathExp -> MathExp -> MathExp)
parseOps op = do
  c <- foldr1 (+++) $ map char op
  return $ opTable M.! c
 where
  opTable =
    M.fromList [('+', Plus), ('-', Minus), ('*', Mult), ('/', Div), ('^', Pow)]

parsePlusAndMinus, parseMultAndDiv, parsePow
  :: ReadP (MathExp -> MathExp -> MathExp)
parsePlusAndMinus = parseOps ['+', '-']
parseMultAndDiv = parseOps ['*', '/']
parsePow = parseOps ['^']

parseLet :: ReadP MathExp
parseLet = do
  string "let"
  names <- parseNameTuple <++ fmap (: []) parseName
  char '='
  values <- parseMathExpTuple <++ fmap (: []) parseMathExp
  string "in"
  -- This is hacky, I couldn't come up with a cleaner way
  rest <- munch (/= ')')
  let (Right exp) = parse rest
  return $ Let names values exp

parseFlat :: ReadP MathExp
parseFlat = parseNumber +++ parseVarName

parseInParens :: ReadP a -> ReadP a
parseInParens = between (char '(') (char ')')

parseParenExp :: ReadP MathExp
parseParenExp = parseInParens parseMathExp

parsePossiblyNegParen :: ReadP MathExp
parsePossiblyNegParen = parsePossiblyNegExp parseParenExp

parseTuple :: ReadP a -> ReadP [a]
parseTuple p = parseInParens $ p `sepBy` char ','

parseNameTuple :: ReadP [Name]
parseNameTuple = parseTuple parseName

parseMathExpTuple :: ReadP [MathExp]
parseMathExpTuple = parseTuple parseMathExp

parseMathExp :: ReadP MathExp
parseMathExp = chainl1 parseHigher parsePlusAndMinus
 where
  parseHigher = chainl1 parseEvenHigher parseMultAndDiv
  parseEvenHigher = -- I know I took the joke literally but they really are good names
    parsePossiblyNegExp $ chainr1 parseEvenMoreHigher parsePow
  parseEvenMoreHigher = parseLet <++ parseParenExp <++ parseFlat


-- Run the parser on a given string.
--
-- You should not modif y this function. Grading may
-- look for the specific messages below.
parse :: String -> Either String MathExp
parse str' = case (completeParses, incompleteParses) of
  ([(result, "")], _) -> Right result  -- Only complete result.
  ([], []) -> Left $ "No parse."
  ([], _ : _) -> Left $ "Incomplete parse. Unparsed: " ++ show leastRemaining
  (_ : _, _) -> Left $ "Ambiguous parse: " ++ show (map fst completeParses)
 where
  str    = filter (not . isSpace) str'
  parses = readP_to_S parseMathExp str
  (completeParses, incompleteParses) =
    partition (\(_, remaining) -> remaining == "") parses
  leastRemaining = minimumBy (comparing length) . map snd $ incompleteParses
