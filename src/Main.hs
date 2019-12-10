module Main where
import Day8.Day8
import Paths_AoC2019

import Data.List.Split

main :: IO ()
main = do
    fileName <- getDataFileName "day8.txt"
    input <- readFile fileName
    let test = "0222112222120000"
    mapM_ putStrLn $ part2 25 6 $ allInts input

listOfInts :: String -> [Integer]
listOfInts content = map read (lines content)

csvInts :: String -> [Int]
csvInts content = map (fromIntegral . read) $ splitOn "," content

csvStrings :: String -> [String]
csvStrings content = splitOn "," content

allInts :: String -> [Integer]
allInts = map (read . (:[]))