module Main where

import Data.List.Split
import Day18.Day18
import Paths_AoC2019

main :: IO ()
main = do
  fileName1 <- getDataFileName "day18.1.txt"
  fileName2 <- getDataFileName "day18.2.txt"
  fileName3 <- getDataFileName "day18.3.txt"
  fileName4 <- getDataFileName "day18.4.txt"
  input1 <- readFile fileName1
  input2 <- readFile fileName2
  input3 <- readFile fileName3
  input4 <- readFile fileName4

  print $ part2 [lines input1, lines input2, lines input3, lines input4]

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
