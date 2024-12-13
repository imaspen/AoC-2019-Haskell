module Main where

import Data.List.Split
import Day22.Day22
import Paths_AoC2019

main :: IO ()
main = do
  testFileName <- getDataFileName "day22.test.txt"
  testInput <- readFile testFileName

  fileName <- getDataFileName "day22.txt"
  input <- readFile fileName

  putStrLn "Part 1 Test:"
  print $ [9, 2, 5, 8, 1, 4, 7, 0, 3, 6] == part1List 10 (lines testInput)

  putStrLn ""
  putStrLn "Part 1:"
  print $ part1 10007 (lines input)

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
