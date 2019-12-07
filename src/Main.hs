module Main where
import Day5.Day5
import Paths_AoC2019

import Data.List.Split

main :: IO ()
main = do
    fileName <- getDataFileName "day5.txt"
    input <- readFile fileName
    let program = csvInts input
    mapM_ (putStrLn . show) $ part1 program [1]

listOfInts :: String -> [Integer]
listOfInts content = map read (lines content)

csvInts :: String -> [Int]
csvInts content = map (fromIntegral . read) $ splitOn "," content

csvStrings :: String -> [String]
csvStrings content = splitOn "," content
