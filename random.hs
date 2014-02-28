module Random where

--import Data.Random.Source
import Data.Random.Source.PureMT
import Data.Random.Sample
import Data.Random.Distribution.Exponential
import Data.Random.Distribution.Uniform
import Data.Random
import Text.Printf

import Control.Applicative
import Control.Monad.State

import System.Random


failuresWith p u = floor (log u / log (1 - p))

uniformGeometric :: (MonadRandom m) => Double -> m Int
uniformGeometric p = sample $ failuresWith p <$> stdUniform

runRandTIO m = m

runRandIO :: IO a -> IO a
runRandIO m = m

runRandT m g = evalStateT m g

runRand m g = evalState m g


main = do
  b <- runRandIO $ uniformGeometric 0.01
  printf "%d\n" b
  putStrLn "\n"

