module Main where

import Data.List.Split
import Day15.Day15
import Paths_AoC2019

main :: IO ()
main = do
  fileName <- getDataFileName "day15.txt"
  input <- readFile fileName

  putStrLn "Part 1:"
  print $ part1 $ csvInts input

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
