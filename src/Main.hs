module Main where

import Data.List.Split
import Day16.Day16
import Paths_AoC2019

main :: IO ()
main = do
  fileName <- getDataFileName "day16.txt"
  input <- readFile fileName
  fileNameTest1 <- getDataFileName "day16.test.1.txt"
  inputTest1 <- readFile fileNameTest1
  fileNameTest2 <- getDataFileName "day16.test.2.txt"
  inputTest2 <- readFile fileNameTest2
  fileNameTest3 <- getDataFileName "day16.test.3.txt"
  inputTest3 <- readFile fileNameTest3

  putStrLn "Part 1 Tests:"
  print $ 24176176 == part1 (allInts inputTest1)
  print $ 73745418 == part1 (allInts inputTest2)
  print $ 52432133 == part1 (allInts inputTest3)

  putStrLn ""
  putStrLn "Part 1:"
  print $ part1 $ allInts input

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
