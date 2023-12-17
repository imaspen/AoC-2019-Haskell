module Main where

import Data.List.Split
import Day11.Day11
import Paths_AoC2019

main :: IO ()
main = do
  fileName <- getDataFileName "day11.txt"
  input <- readFile fileName
  print $ part1 $ csvInts input
  putStr $ part2 $ csvInts input

listOfInts :: String -> [Integer]
listOfInts content = map read (lines content)

csvInts :: String -> [Int]
csvInts content = map (fromIntegral . read) $ splitOn "," content

csvStrings :: String -> [String]
csvStrings = splitOn ","

allInts :: String -> [Integer]
allInts = map (read . (: []))
