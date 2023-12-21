module Main where

import Data.List.Split
import Day13.Day13
import Paths_AoC2019

main :: IO ()
main = do
  fileName <- getDataFileName "day13.txt"
  input <- readFile fileName
  let memory = csvInts input

  putStrLn "Part 1:"
  print $ part1 memory

  putStrLn "Part 2:"

  print $ part2 memory

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
