module Main where

import Data.List.Split
import Day12.Day12
import Paths_AoC2019

main :: IO ()
main = do
  fileName <- getDataFileName "day12.txt"
  input <- readFile fileName
  let points = listOfPoints input

  let test = [[-8, -10, 0], [5, 5, 10], [2, -7, 3], [9, -8, -3]]
  -- let test = [[-1, 0, 2], [2, -10, -7], [4, -8, 8], [3, 5, -1]]

  putStrLn "Part 1 test:"
  print $ part1 100 test

  putStrLn "Part 1 result:"
  print $ part1 1000 points

  putStrLn ""

  putStrLn "Part 2 test:"
  print $ part2 test

  putStrLn "Part 2 result:"
  print $ part2 points

listOfInts :: String -> [Integer]
listOfInts content = map read (lines content)

listOfPoints :: String -> [[Int]]
listOfPoints content = map (map (read . last . splitOn "=") . splitOn ", " . tail . init) (lines content)

csvInts :: String -> [Int]
csvInts content = map (fromIntegral . read) $ splitOn "," content

csvStrings :: String -> [String]
csvStrings = splitOn ","

allInts :: String -> [Integer]
allInts = map (read . (: []))
