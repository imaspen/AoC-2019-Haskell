module Day8.Day8 where

import Data.List
import Data.List.Split

part1 :: Integer -> Integer -> [Integer] -> Integer
part1 x y values = layerValue minLayer
    where
        layers = chunksOf (fromIntegral (x * y)) values
        minLayer = minimumBy compare0s layers

layerValue :: [Integer] -> Integer
layerValue layer = (countNs 1 layer) * (countNs 2 layer)

compare0s :: [Integer] -> [Integer] -> Ordering
compare0s x y = compare (count0s x) (count0s y)

count0s :: [Integer] -> Integer
count0s = countNs 0

countNs :: Integer -> [Integer] -> Integer
countNs n = toInteger . length . filter (==n)