module Main where
import           RandState
import           System.Environment             ( getArgs )
import           System.Random.Stateful         ( newStdGen )

-- | Returns a random pair of numbers within a specified range
randPair :: (Double, Double) -> RandState (Double, Double)
randPair (low, high) = do
  r1 <- randR (low, high)
  r2 <- randR (low, high)
  pure (r1, r2)

-- | Calculates the distance to origin for an x y ordered "pair"
distanceToOrigin :: Floating a => a -> a -> a
distanceToOrigin x y = sqrt $ x ^ 2 + y ^ 2

-- | Returns True if randomly chosen point from square is inside circumscribed circle
piTrial :: RandState Bool
piTrial = do
  (x, y) <- randPair (-1.0, 1.0)
  pure $ distanceToOrigin x y <= 1.0

-- | Perform n trials of the RandState function provided as the second argument,
--   and give back the number of successful trials
bernoulliTrials :: Int -> RandState Bool -> RandState Int
bernoulliTrials 0      _    = pure 0
bernoulliTrials nTimes test = do
  result <- test
  rest   <- bernoulliTrials (nTimes - 1) test
  pure $ fromEnum result + rest

-- | Approximate pi using n randomly chosen points
approxPi :: Int -> RandState Double
approxPi n = do
  result <- bernoulliTrials n piTrial
  pure $ fromIntegral result / fromIntegral n * 4

main :: IO ()
main = do
  nTimes <- read . head <$> getArgs
  gen    <- newStdGen
  print $ runRandom (approxPi nTimes) gen
