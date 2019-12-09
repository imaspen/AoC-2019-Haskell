module Main where
import Day6.Day6
import Paths_AoC2019

import Data.List.Split

main :: IO ()
main = do
    fileName <- getDataFileName "day6.txt"
    input <- readFile fileName
    putStrLn . show $ part1 $ lines input

listOfInts :: String -> [Integer]
listOfInts content = map read (lines content)

csvInts :: String -> [Int]
csvInts content = map (fromIntegral . read) $ splitOn "," content

csvStrings :: String -> [String]
csvStrings content = splitOn "," content
