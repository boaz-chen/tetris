module Util where

import           System.Random
import           Data.Time.Clock
import           Data.Time.Format

getSeedFromCurrentTime :: IO Int
getSeedFromCurrentTime = do
  currentTime <- getCurrentTime
  let seconds = formatTime defaultTimeLocale "%s" currentTime
  return $ read seconds

getGoodStdGen :: IO StdGen
getGoodStdGen = do
  seed <- getSeedFromCurrentTime
  let stdGen = mkStdGen seed
  return stdGen
