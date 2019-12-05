module Main where
import Day3.Day3
import Paths_AoC2019

import Data.List.Split

main :: IO ()
main = do
    fileName <- getDataFileName "day3.txt"
    input <- readFile fileName
    putStrLn $ show $ part1 $ map csvStrings ["R8,U5,L5,D3", "U7,R6,D4,L4"]
    putStrLn $ show $ part1 $ map csvStrings ["R75,D30,R83,U83,L12,D49,R71,U7,L72", "U62,R66,U55,R34,D71,R55,D58,R83"]
    putStrLn $ show $ part1 $ map csvStrings ["R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51", "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"]
    putStrLn $ show $ part1 $ map csvStrings (lines input) 

listOfInts :: String -> [Integer]
listOfInts content = map read (lines content)

csvInts :: String -> [Int]
csvInts content = map (fromIntegral . read) $ splitOn "," content

csvStrings :: String -> [String]
csvStrings content = splitOn "," content
