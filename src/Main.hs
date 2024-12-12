module Main where

import Data.List.Split
import Day20.Day20
import Paths_AoC2019

main :: IO ()
main = do
  testFileName <- getDataFileName "day20.test.1.txt"
  testInput <- readFile testFileName

  testFileName2 <- getDataFileName "day20.test.2.txt"
  testInput2 <- readFile testFileName2

  fileName <- getDataFileName "day20.txt"
  input <- readFile fileName

  putStrLn "Part 1 Tests:"
  print $ 58 == part1 (lines testInput) ((8, 8), (26, 28))

  putStrLn ""
  putStrLn "Part 1:"
  print $ part1 (lines input) ((32, 32), (86, 90))

  putStrLn ""
  putStrLn "Part 2 Tests:"
  print $ part2 (lines testInput2) ((8, 8), (36, 28))

  putStrLn ""
  putStrLn "Part 2:"
  print $ part2 (lines input) ((32, 32), (86, 90))

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
