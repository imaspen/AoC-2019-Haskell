module Main where
import Day2.Day2
import Paths_AoC2019

import Data.List.Split

main :: IO ()
main = do
    fileName <- getDataFileName "day2.txt"
    input <- readFile fileName
    let opCodes = csvInts input
    --let opCodes = [1,9,10,3,2,3,11,0,99,30,40,50]
    --let opCodes = [1,0,0,0,99]
    putStrLn $ show $ part1 opCodes

listOfInts :: String -> [Integer]
listOfInts content = map read (lines content)

csvInts :: String -> [Int]
csvInts content = map (fromIntegral . read) $ splitOn "," content
