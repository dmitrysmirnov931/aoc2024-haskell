module Main (main) where

import qualified Days.Day01 as Day01 (runDay)
import qualified Days.Day02 as Day02 (runDay)

main :: IO ()
main = do
  Day01.runDay
  Day02.runDay
  
