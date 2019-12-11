module Main where
import Day9.Day9
import Paths_AoC2019

import Data.List.Split

main :: IO ()
main = do
    fileName <- getDataFileName "day9.txt"
    input <- readFile fileName
    let test1 = [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99]
    let test2 = [1102,34915192,34915192,7,4,7,99,0]
    let test3 = [104,1125899906842624,99]
    putStrLn $ show $ part1 $ csvInts input

listOfInts :: String -> [Integer]
listOfInts content = map read (lines content)

csvInts :: String -> [Int]
csvInts content = map (fromIntegral . read) $ splitOn "," content

csvStrings :: String -> [String]
csvStrings content = splitOn "," content

allInts :: String -> [Integer]
allInts = map (read . (:[]))