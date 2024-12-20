module Main where

import Data.List.Split
import Day24.Day24
import Paths_AoC2019

main :: IO ()
main = do
  testFileName <- getDataFileName "day24.test.txt"
  testInput <- readFile testFileName
  fileName <- getDataFileName "day24.txt"
  input <- readFile fileName

  print $ part2 testInput 10
  print $ part2 input 200

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
