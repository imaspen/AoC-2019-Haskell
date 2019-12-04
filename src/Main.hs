module Main where
import Day1.Day1
import Paths_AoC2019
main :: IO ()
main = do
    fileName <- getDataFileName "day1.txt"
    input <- readFile fileName
    let masses = listOfInts input
    putStrLn $ show $ part1 masses

listOfInts :: String -> [Integer]
listOfInts content = map read (lines content)
