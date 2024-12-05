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

  putStrLn "Part 2 Tests:"
  print $ 84462026 == part2 (allInts inputTest1)
  print $ 78725270 == part2 (allInts inputTest2)
  print $ 53553731 == part2 (allInts inputTest3)

  putStrLn ""
  putStrLn "Part 2:"
  print $ part2 $ allInts input

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
