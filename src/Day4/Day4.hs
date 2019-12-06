module Day4.Day4 where

import Data.Char

part1 :: (Integer, Integer) -> Int
part1 (bottom, top) = do
    let attempts = map splitNum [bottom..top]
    length $ filter isValid attempts

part2 :: (Integer, Integer) -> Int
part2 (bottom, top) = do
    let attempts = map splitNum [bottom..top]
    length $ filter isValid' attempts

splitNum :: Integer -> [Integer]
splitNum num = map (\c -> read [c]) $ show num

isValid :: [Integer] -> Bool
isValid list = (increases list) && (containsDouble list)

isValid' :: [Integer] -> Bool
isValid' list = (increases list) && (containsDouble' list)

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

containsDouble' :: [Integer] -> Bool
containsDouble' list = (length $ filter (\(_, count) -> count == 2) $ foldr repeats [] list) > 0

repeats :: Integer -> [(Integer, Integer)] -> [(Integer, Integer)]
repeats val [] = [(val, 1)]
repeats val ((previous, occurences) : rest)
    | val == previous = ((previous, occurences + 1) : rest)
    | otherwise       = ((val, 1) : (previous, occurences) : rest)
