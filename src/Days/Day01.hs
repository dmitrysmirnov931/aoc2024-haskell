module Days.Day01 (runDay) where

import Data.List (sort)
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)
import qualified Data.Map.Strict as Map

runDay :: IO ()
runDay = do
  task1 "input/Day01.txt"
  task2 "input/Day01.txt"


task1 :: FilePath -> IO ()
task1 filePath = do
  contents <- readFile filePath
  let pairs = mapMaybe parseLine (lines contents)
      (list1, list2) = unzip pairs
      distances = zipWith (\a b -> abs (b - a)) (sort list1) (sort list2)
      result = sum distances
  putStrLn $ "Day01, task1: " ++ show result

task2 :: FilePath -> IO ()
task2 filePath = do
  contents <- readFile filePath
  let pairs = mapMaybe parseLine (lines contents)
      (list1, list2) = unzip pairs
      frequencyMap = Map.fromListWith (+) [(x, 1) | x <- list2]
      distances = [x * Map.findWithDefault 0 x frequencyMap | x <- list1]
      result = sum distances
  putStrLn $ "Day01, task2: " ++ show result


parseLine :: String -> Maybe (Int, Int)
parseLine line = 
  case words line of
    [a, b] -> (,) <$> readMaybe a <*> readMaybe b
    _ -> Nothing
