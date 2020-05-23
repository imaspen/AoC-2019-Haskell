module Main where
import Day10.Day10
import Paths_AoC2019

import Data.List.Split

main :: IO ()
main = do
    fileName <- getDataFileName "day10.txt"
    testFileName <- getDataFileName "day10test.txt"
    input <- readFile fileName
    test3 <- readFile testFileName
    let test1 = [".#..#",".....","#####","....#","...##"]
    let test2 = ["..#..","..#..","#.#.#",".###.","#####"]
    putStrLn $ show $ part2 $ lines test3
    putStrLn $ show $ part2 $ lines input


listOfInts :: String -> [Integer]
listOfInts content = map read (lines content)

csvInts :: String -> [Int]
csvInts content = map (fromIntegral . read) $ splitOn "," content

csvStrings :: String -> [String]
csvStrings content = splitOn "," content

allInts :: String -> [Integer]
allInts = map (read . (:[]))