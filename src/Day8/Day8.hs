module Day8.Day8 where

import Data.List
import Data.List.Split

part1 :: Integer -> Integer -> [Integer] -> Integer
part1 x y values = layerValue minLayer
    where
        layers = chunksOf (fromIntegral (x * y)) values
        minLayer = minimumBy compare0s layers

part2 :: Integer -> Integer -> [Integer] -> [String]
part2 x y values = map (map toArt) $ chunksOf (fromIntegral x) $ combination [] layers
    where
        layers = chunksOf (fromIntegral (x * y)) values
        
toArt :: Integer -> Char
toArt 0 = ' '
toArt 1 = 'X'
toArt 2 = 'E'

layerValue :: [Integer] -> Integer
layerValue layer = (countNs 1 layer) * (countNs 2 layer)

compare0s :: [Integer] -> [Integer] -> Ordering
compare0s x y = compare (count0s x) (count0s y)

count0s :: [Integer] -> Integer
count0s = countNs 0

countNs :: Integer -> [Integer] -> Integer
countNs n = toInteger . length . filter (==n)

combination :: [Integer] -> [[Integer]] -> [Integer]
combination [] (l : rest) = combination l rest
combination l [] = l
combination l1 (l2 : rest) = combination (combineLayers l1 l2) rest

combineLayers :: [Integer] -> [Integer] -> [Integer]
combineLayers a b = map combinePixels $ zip a b

combinePixels :: (Integer, Integer) -> Integer
combinePixels (a, b)
    | a == 2 = b
    | otherwise = a