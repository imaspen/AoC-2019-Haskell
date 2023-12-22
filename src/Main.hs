module Main where

import Data.List.Split
import Day14.Day14
import Paths_AoC2019

main :: IO ()
main = do
  fileName <- getDataFileName "day14.txt"
  test1 <- getDataFileName "day14.test.1.txt"
  test2 <- getDataFileName "day14.test.2.txt"
  test3 <- getDataFileName "day14.test.3.txt"
  test4 <- getDataFileName "day14.test.4.txt"
  test5 <- getDataFileName "day14.test.5.txt"
  input <- readFile fileName
  testInput1 <- readFile test1
  testInput2 <- readFile test2
  testInput3 <- readFile test3
  testInput4 <- readFile test4
  testInput5 <- readFile test5

  putStrLn "Part 1 tests:"
  print $ 31 == part1 (lines testInput1)
  print $ 165 == part1 (lines testInput2)
  print $ 13312 == part1 (lines testInput3)
  print $ 180697 == part1 (lines testInput4)
  print $ 2210736 == part1 (lines testInput5)

  putStrLn ""
  putStrLn "Part 1:"
  print $ part1 $ lines input

  putStrLn ""
  putStrLn "Part 2 tests:"
  print $ 82892753 == part2 (lines testInput3)
  print $ 5586022 == part2 (lines testInput4)
  print $ 460664 == part2 (lines testInput5)

  putStrLn ""
  putStrLn "Part 2:"
  print $ part2 $ lines input

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
