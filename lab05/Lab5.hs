import           Control.Applicative            ( Applicative(liftA2) )
import           RandState
import           System.Environment             ( getArgs )
import           System.Random                  ( Random(randomR)
                                                , StdGen
                                                , newStdGen
                                                )

import           Control.Monad                  ( replicateM )
import           Data.Function                  ( on )
import qualified Data.List                     as List
import           Data.Tuple

-- Rolling two dices 

-- | Returns the sum of rolling two dices as a monadic RandState Instance
rollTwoDice :: RandState Int
rollTwoDice = liftA2 (+) (randR (1, 6)) (randR (1, 6))

-- Shuffling cards

-- Data types to represent playing cards
data CardValue
    = King
    | Queen
    | Jack
    | NumberCard Int  -- From 1 to 10
    deriving (Show, Eq)

data CardSuit
    = Hearts
    | Diamonds
    | Spades
    | Clubs
    deriving (Show, Eq)

data PlayingCard = PlayingCard CardValue CardSuit
  deriving Eq

type Deck = [PlayingCard]


instance Show PlayingCard where
  show (PlayingCard value suit) = valueStr value ++ suitStr suit
   where
    suitStr Hearts   = "\x1B[31m♥\x1B[0m" -- red in terminal
    suitStr Diamonds = "\x1B[31m♦\x1B[0m" -- red in terminal
    suitStr Spades   = "♠"
    suitStr Clubs    = "♣"
    valueStr King           = "K"
    valueStr Queen          = "Q"
    valueStr Jack           = "J"
    valueStr (NumberCard n) = show n


-- | fullCardDeck is a deck of cards, 52 in total, with a King, a Queen,
--   a Jack and NumberCards from 1 to 10 for each suit.
fullCardDeck :: Deck
fullCardDeck = [ PlayingCard v s | v <- allVals, s <- allSuits ]
 where
  allVals  = King : Queen : Jack : [ NumberCard i | i <- [1 .. 10] ]
  allSuits = [Hearts, Diamonds, Spades, Clubs]

-- | Removes a random card from a deck and returns the pair of the card removed and the deck without the card
removeCard :: Deck -> RandState (PlayingCard, Deck)
removeCard deck = do
  randomIndex <- randR (0, length deck - 1)
  let card = deck !! randomIndex
  pure (card, deleteAt randomIndex deck)
 where
  deleteAt index lst = let (lft, _ : rgt) = splitAt index lst in lft ++ rgt

-- | Given a deck, the function returns a randomly shuffled deck
shuffleDeck :: Deck -> RandState Deck
shuffleDeck []   = pure []
shuffleDeck deck = do
  (card, deck') <- removeCard deck
  shuffledDeck  <- shuffleDeck deck'
  pure (card : shuffledDeck)

shuffleADeck :: RandState Deck
shuffleADeck = shuffleDeck fullCardDeck

-- UNCOMMENT THE FOLLOWING BEFORE SUBMISSION --

-- | Evaluates and outputs to the std output the result of a RandState nTimes times
doNTimes :: (Show a) => RandState a -> Int -> StdGen -> IO ()
doNTimes _    0      _   = pure ()
doNTimes what nTimes gen = do
  let (result, gen') = runRandState what gen
  print result
  doNTimes what (nTimes - 1) gen'

-- | Shuffles the full deck nTimes times and prints the result of each time to the std output
shuffleNTimes :: Int -> StdGen -> IO ()
shuffleNTimes = doNTimes shuffleADeck

-- | Prints the sum of two dice rolls nTimes times to the std output
rollTwoDiceNTimes :: Int -> StdGen -> IO ()
rollTwoDiceNTimes = doNTimes rollTwoDice

-- BESIDES UNCOMMENTING, DO NOT MODIFY BELOW THIS LINE --

usage :: String
usage =
  "Lab 5: Randomizer\n"
    ++ "\n"
    ++ "$ ./Lab5 shuffle 600      # 600 times: output a full deck shuffle\n"
    ++ "$ ./Lab5 rollTwoDice 800  # 800 times: output the sum of rolling two dice\n"
    ++ "\n"

main :: IO ()
main = do
  gen  <- newStdGen
  args <- getArgs
  case args of
    ["shuffle"    , nTimes] -> shuffleNTimes (read nTimes) gen
    ["rollTwoDice", nTimes] -> rollTwoDiceNTimes (read nTimes) gen
    _                       -> putStrLn usage

-- | Randomness test for shuffle, to be executed using ghci
testRandomness :: IO ()
testRandomness = do
  gen <- newStdGen
  let
    nCards  = 10
    trials  = 10000
    cards   = take nCards fullCardDeck
    results = fst $ runRandState (replicateM trials (shuffleDeck cards)) gen
    stats   = map
      (\x -> [ fromEnum (cards !! i == x !! i) | i <- [0 .. nCards] ])
      results
    propOccurence =
      map (\x -> fromIntegral (x * nCards) / fromIntegral trials)
        $ foldr (zipWith (+)) (replicate nCards 0) stats
  print propOccurence -- <-- a list whose values are close to 1 and whose sum is nCards
