module RandState where

import           System.Random                  ( Random(random, randomR)
                                                , StdGen
                                                )

newtype RandState a = RandState {
  runRandState :: StdGen -> (a, StdGen)
}

instance Functor RandState where
  fmap f rs = rs >>= return . f
instance Applicative RandState where
  pure a = RandState $ (,) a
  mf <*> ma = do
    f <- mf
    f <$> ma

instance Monad RandState where
  mra >>= f = RandState $ \gen ->
    let (a, gen')  = runRandState mra gen
        mf         = f a
        (b, gen'') = runRandState mf gen'
    in  (b, gen'')


{- primitive manipulation functions -}

get :: RandState StdGen
get = RandState $ \gen -> (gen, gen)

put :: StdGen -> RandState ()
put gen = RandState $ const ((), gen)

-- runRandom runs a RandState monad, given an initial random number generator
-- runRandom is equivalent to evalState
runRandom :: RandState a -> StdGen -> a
runRandom (RandState f) s = fst $ f s

-- rand is a helper function that generates a random instance of any
--  type in the Random class, using the RandState monad.
rand :: Random a => RandState a
rand = RandState random

-- | Generates a random number within a range
randR :: Random a => (a, a) -> RandState a
randR = RandState . randomR

