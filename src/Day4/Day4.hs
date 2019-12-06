module Day4.Day4 where

import Data.Char

part1 :: (Integer, Integer) -> Int
part1 (bottom, top) = do
    let attempts = map splitNum [bottom..top]
    length $ filter isValid attempts

splitNum :: Integer -> [Integer]
splitNum num = map (\c -> read [c]) $ show num

isValid :: [Integer] -> Bool
isValid list = (increases list) && (containsDouble list)

increases :: [Integer] -> Bool
increases (a : b : []) = a <= b
increases (a : b : remaining)
    | a <= b    = increases (b : remaining)
    | otherwise = False

containsDouble :: [Integer] -> Bool
containsDouble (a : b : []) = a == b
containsDouble (a : b : remaining)
    | a == b    = True
    | otherwise = containsDouble (b : remaining)