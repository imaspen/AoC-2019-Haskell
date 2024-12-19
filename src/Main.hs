module Main where

import Data.List.Split
import Day23.Day23
import Paths_AoC2019

main :: IO ()
main = do
  fileName <- getDataFileName "day23.txt"
  input <- readFile fileName

  print $ part2 $ csvInts input

listOfInts :: String -> [Integer]
listOfInts content = map read (lines content)

listOfPoints :: String -> [[Int]]
listOfPoints content = map (map (read . last . splitOn "=") . splitOn ", " . tail . init) (lines content)

csvInts :: String -> [Int]
csvInts content = map (fromIntegral . read) $ splitOn "," content

csvStrings :: String -> [String]
csvStrings = splitOn ","

allInts :: String -> [Integer]
allInts content = map (read . (: [])) $ head $ lines content
