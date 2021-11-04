import RandState
import System.Environment
import System.IO
import System.Random

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

data PlayingCard =
    PlayingCard CardValue CardSuit
    deriving (Eq)

type Deck = [PlayingCard]


instance Show PlayingCard where
    show (PlayingCard value suit) =
        valueStr value ++ suitStr suit
        where
            suitStr Hearts   = "\x1B[31m♥\x1B[0m" -- red in terminal
            suitStr Diamonds = "\x1B[31m♦\x1B[0m" -- red in terminal
            suitStr Spades   = "♠"
            suitStr Clubs    = "♣"
            -- suitStr Hearts   = "H"  -- uncomment if you don't have Unicode
            -- suitStr Diamonds = "D"
            -- suitStr Spades   = "S"
            -- suitStr Clubs    = "C"
            valueStr King           = "K"
            valueStr Queen          = "Q"
            valueStr Jack           = "J"
            valueStr (NumberCard n) = show n


-- fullCardDeck is a deck of cards, 52 in total, with a King, a Queen,
-- a Jack and NumberCards from 1 to 10 for each suit.
fullCardDeck :: Deck
fullCardDeck =
    [ PlayingCard v s | v <- allVals, s <- allSuits ]
    where
        allVals  = King : Queen : Jack : [ NumberCard i | i <- [1..10] ]
        allSuits = [Hearts, Diamonds, Spades, Clubs]

-- basic random functions

randR :: Random a => (a, a) -> RandState a
randR = undefined

-- -- UNCOMMENT THE FOLLOWING BEFORE SUBMISSION --
--
--
-- shuffleNTimes :: Int -> StdGen -> IO ()
-- shuffleNTimes nTimes gen =
--     undefined
--
-- rollTwoDiceNTimes :: Int -> StdGen -> IO ()
-- rollTwoDiceNTimes nTimes gen =
--     undefined
--
--
-- -- BESIDES UNCOMMENTING, DO NOT MODIFY BELOW THIS LINE --
--
-- usage :: String
-- usage =
--     "Lab 5: Randomizer\n" ++
--     "\n" ++
--     "$ ./Lab5 shuffle 600      # 600 times: output a full deck shuffle\n" ++
--     "$ ./Lab5 rollTwoDice 800  # 800 times: output the sum of rolling two dice\n" ++
--     "\n"
--
-- main :: IO ()
-- main = do
--     gen  <- newStdGen
--     args <- getArgs
--     case args of
--         ["shuffle",     nTimes] -> shuffleNTimes     (read nTimes) gen
--         ["rollTwoDice", nTimes] -> rollTwoDiceNTimes (read nTimes) gen
--         _                       -> putStrLn usage
