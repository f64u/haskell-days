-- This is example code.
--
-- You may remove this file before your final submission.
--

module RandStateExamples where

import           RandState
import           System.Random

-- Example of how to use the RandState monad within the IO monad
randStateExample1 :: IO Int
randStateExample1 = do
    -- create new random number generator
    gen <- newStdGen
    -- run rand with the new random number generator.  The fact that the
    --  type of stateExample1 is IO Int ensures that rand is forced to generate
    --  a random value of type Int
    let r = runRandom rand gen
    return r


-- run e.g. StateExample> stateExample2 10
randStateExample2 :: Int -> IO [Double]
randStateExample2 n = do
    gen <- newStdGen
    let rs = runRandom (rList n []) gen
    return rs
    where
        rList :: Int -> [Double] -> RandState [Double]
        rList 0 xs = do
            return xs
        rList n xs = do
            x <- rand
            -- recurse to build rest of list
            rList (n-1) (x:xs)


