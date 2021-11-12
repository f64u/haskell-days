-- Write your parser in this file.

module Lab6
  ( Name
  , Number
  , TopLevelExp(..)
  , MathExp(..)
  , parse
  ) where

import           Data.Char
import           Data.List
import qualified Data.Map.Strict               as M
import           Data.Ord
import           Text.ParserCombinators.ReadP

type Name = String  -- Variable names are strings.
type Number = Int     -- The kind of number in our language.


-- A top-level expression is either:
--
-- 1) A bare mathematical expression:
--
-- 4 + (2*5)
--
-- 2) A let-binding followed by an expression to evaluate:
--
-- let x = 5 in x + 4
--
-- let (x1, y1, x2, y2) = (5,5,10,10) in (y2-y1)*(y2-y1) + (x2-x1)*(x2-x1)
--
-- You can assume that the tuples on either side of the = sign are
-- always the same length--if not it will be treated as an eval error.

data TopLevelExp
    = MathTLE MathExp
    | LetTLE [Name] [MathExp] MathExp
    deriving (Eq, Show)


-- A math expression is a number, a variable,
-- a negation of a math expression, or any of
-- the four major operations plus power on a
-- pair of math expressions.
--
-- In the actual parser:
--   1. Precedence should be standard order of operations.
--   2. Negation should only precede a number, a variable, or
--      a parenthetical expression.
--   3. A variable starts with a lowercase letter and after
--      the first letter is any alphanumeric character a-z A-Z 0-9.
--
-- Your parser does _not_ need an explicit tokenization step like Lab 3.
-- In the functional parsing paradigm, tokenization+parsing occur
-- simultaneously.

data MathExp
    = Number Number
    | Var    Name
    | Neg    MathExp
    | Plus   MathExp MathExp
    | Minus  MathExp MathExp
    | Mult   MathExp MathExp
    | Div    MathExp MathExp
    | Pow    MathExp MathExp
    deriving (Eq, Show)

spaceableParser :: ReadP a -> ReadP a
spaceableParser = between skipSpaces skipSpaces

spaceableChar :: Char -> ReadP Char
spaceableChar = spaceableParser . char

parsePossiblyNegExp :: ReadP MathExp -> ReadP MathExp
parsePossiblyNegExp p = (Neg <$> (spaceableChar '-' *> p)) +++ p

parseNumber :: ReadP MathExp
parseNumber = Number . read <$> (skipSpaces *> many1 (satisfy isDigit))

parsePossiblyNegNumber :: ReadP MathExp
parsePossiblyNegNumber = parsePossiblyNegExp parseNumber

parseName :: ReadP [Char]
parseName = (:) <$> satisfy isLower <*> many (satisfy isAlphaNum)

parseVarName :: ReadP MathExp
parseVarName = Var <$> parseName

parsePossiblyNegVarName :: ReadP MathExp
parsePossiblyNegVarName = parsePossiblyNegExp parseVarName

parseOps :: [Char] -> ReadP (MathExp -> MathExp -> MathExp)
parseOps op = do
  c <- foldr1 (+++) $ map spaceableChar op
  return $ opTable M.! c
 where
  opTable =
    M.fromList [('+', Plus), ('-', Minus), ('*', Mult), ('/', Div), ('^', Pow)]

parsePlusAndMinus, parseMultAndDiv, parsePow
  :: ReadP (MathExp -> MathExp -> MathExp)
parsePlusAndMinus = parseOps ['+', '-']
parseMultAndDiv = parseOps ['*', '/']
parsePow = parseOps ['^']

parseFlat :: ReadP MathExp
parseFlat = parsePossiblyNegNumber +++ parsePossiblyNegVarName

parseFlatPow :: ReadP MathExp
parseFlatPow = parseNumber +++ parseVarName

parseInParens :: ReadP a -> ReadP a
parseInParens = between (spaceableChar '(') (spaceableChar ')')

parseParenExp :: ReadP MathExp
parseParenExp = parseInParens parseMathExp

parsePossiblyNegParen :: ReadP MathExp
parsePossiblyNegParen = parsePossiblyNegExp parseParenExp

parseMathExp :: ReadP MathExp
parseMathExp = chainl1 (parseHigher <++ parseFlat) parsePlusAndMinus
 where
  parseHigher     = chainl1 (parseEvenHigher <++ parseFlat) parseMultAndDiv
  parseEvenHigher = parsePossiblyNegExp -- I know I took the joke literally but they really are good names
    $ chainr1 (parsePossiblyNegParen <++ parseFlatPow) parsePow


parseTuple :: ReadP a -> ReadP [a]
parseTuple p = parseInParens $ p `sepBy` spaceableChar ','

parseNameTuple :: ReadP [Name]
parseNameTuple = parseTuple parseName

parseMathExpTuple :: ReadP [MathExp]
parseMathExpTuple = parseTuple parseMathExp

parseMathTLE :: ReadP TopLevelExp
parseMathTLE = MathTLE <$> parseMathExp


parseLetTLE :: ReadP TopLevelExp
parseLetTLE = do
  spaceableParser $ string "let"
  names <- parseNameTuple <++ fmap (: []) parseName
  spaceableChar '='
  values <- parseMathExpTuple <++ fmap (: []) parseMathExp
  spaceableParser $ string "in"
  LetTLE names values <$> parseMathExp


parseTLE :: ReadP TopLevelExp
parseTLE = do
  tle <- parseLetTLE +++ parseMathTLE
  skipSpaces
  return tle


-- Run the parser on a given string.
--
-- You should not modif y this function. Grading may
-- look for the specific messages below.
parse :: String -> Either String TopLevelExp
parse str = case (completeParses, incompleteParses) of
  ([(result, "")], _    ) -> Right result  -- Only complete result.
  ([]            , []   ) -> Left $ "No parse."
  ([], _ : _) -> Left $ "Incomplete parse. Unparsed: " ++ show leastRemaining
  (_ : _         , _    ) -> Left $ "Ambiguous parse: " ++ show completeParses
 where
  parses = readP_to_S parseTLE str
  (completeParses, incompleteParses) =
    partition (\(_, remaining) -> remaining == "") parses
  leastRemaining = minimumBy (comparing length) . map snd $ incompleteParses
