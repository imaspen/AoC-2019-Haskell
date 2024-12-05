module Day16.Day16 where

import qualified Data.List as List
import Debug.Trace (traceShow, traceShowId)
import GHC.Num (integerToInt)

part1 :: [Integer] -> Int
part1 input = genInt 8 $ List.foldr (\_ a -> calculateEntries a) input [1 .. 100]

part2 :: [Integer] -> Int
part2 input =
  let signal = concat $ replicate 10000 input
      offset = genInt 7 input
      transformed = iterate doRTLMod10Sum (drop offset signal) !! 100
   in genInt 8 transformed

doRTLMod10Sum :: [Integer] -> [Integer]
doRTLMod10Sum = scanr1 (\x y -> (x + y) `mod` 10)

genInt :: Int -> [Integer] -> Int
genInt length ints = List.foldl' (\acc head -> acc * 10 + integerToInt head) 0 $ take length ints

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
