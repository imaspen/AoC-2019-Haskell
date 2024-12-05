module Day16.Day16 where

import qualified Data.List as List
import Data.Sequence (mapWithIndex)
import Debug.Trace (traceShow)

part1 :: [Integer] -> Integer
part1 input = genInt $ take 8 $ List.foldr (\_ a -> calculateEntries a) input [1 .. 100]

genInt :: [Integer] -> Integer
genInt = List.foldl' (\acc head -> acc * 10 + head) 0

calculateEntries :: [Integer] -> [Integer]
calculateEntries ints = List.map (calculateEntry ints) $ take (length ints) [1 ..]

calculateEntry :: [Integer] -> Int -> Integer
calculateEntry ints position = getDigit $ sum $ zipWith (*) ints $ generatePattern position

generatePattern :: Int -> [Integer]
generatePattern position =
  let part = List.replicate position
   in drop 1 $ List.cycle $ part 0 ++ part 1 ++ part 0 ++ part (-1)

generatePatternPart :: Int -> Integer -> [Integer]
generatePatternPart = List.replicate

getDigit :: Integer -> Integer
getDigit x = abs x `mod` 10
