module Main where
import Day7.Day7
import Paths_AoC2019

import Data.List.Split

main :: IO ()
main = do
    fileName <- getDataFileName "day7.txt"
    input <- readFile fileName
    let test1 = [3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0]
    let test2 = [3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0]
    putStrLn . show $ part1 $ csvInts input

listOfInts :: String -> [Integer]
listOfInts content = map read (lines content)

csvInts :: String -> [Int]
csvInts content = map (fromIntegral . read) $ splitOn "," content

csvStrings :: String -> [String]
csvStrings content = splitOn "," content
