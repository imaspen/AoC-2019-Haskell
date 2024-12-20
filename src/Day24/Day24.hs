{-# LANGUAGE TupleSections #-}

module Day24.Day24 where

import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace (traceShow, traceShowId)

type Point = (Integer, Integer)

type Point' = (Integer, Integer, Integer)

type Bugs = Set Point

type Bugs' = Set Point'

type Seen = Set Bugs

type Seen' = Set Bugs'

mapSize :: Integer
mapSize = 4

middle :: Integer
middle = mapSize `div` 2

points :: [Point]
points = concatMap (\y -> map (,y) [0 .. mapSize]) [0 .. mapSize]

points' :: [Point']
points' = concatMap (\z -> map (\(x, y) -> (x, y, z)) $ filter (/= (2, 2)) points) [-100 .. 100]

part1 :: String -> Integer
part1 input =
  let inBugs = foldr foldRow Set.empty $ zip [0 .. mapSize] $ map (zip [0 .. mapSize]) $ lines input
      outBugs = stepBugs (Set.singleton inBugs) inBugs
   in foldr (\(p, i) acc -> if Set.member p outBugs then acc + 2 ^ i else acc) 0 $ zip points [0 ..]

part2 :: String -> Int -> Int
part2 input duration =
  let inBugs = Set.map (\(x, y) -> (x, y, 0)) $ foldr foldRow Set.empty $ zip [0 .. mapSize] $ map (zip [0 .. mapSize]) $ lines input
      outBugs = stepBugs' duration inBugs
   in Set.size outBugs

foldRow :: (Integer, [(Integer, Char)]) -> Bugs -> Bugs
foldRow (y, row) bugs = foldr (foldChar y) bugs row

foldChar :: Integer -> (Integer, Char) -> Bugs -> Bugs
foldChar y (x, '#') bugs = Set.insert (x, y) bugs
foldChar _ _ bugs = bugs

stepBugs :: Seen -> Bugs -> Bugs
stepBugs seen bugs =
  let newBugs = foldr (updatePoint bugs) Set.empty points
   in if Set.member newBugs seen
        then newBugs
        else stepBugs (Set.insert newBugs seen) newBugs

stepBugs' :: Int -> Bugs' -> Bugs'
stepBugs' 0 bugs = bugs
stepBugs' steps bugs =
  let newBugs = foldr (updatePoint' bugs) Set.empty points'
   in stepBugs' (steps - 1) newBugs

updatePoint :: Bugs -> Point -> Bugs -> Bugs
updatePoint bugs point newBugs =
  let neighbors = countNeighbors bugs point
   in if Set.member point bugs
        then
          if neighbors /= 1
            then Set.delete point newBugs
            else Set.insert point newBugs
        else
          if neighbors == 1 || neighbors == 2
            then Set.insert point newBugs
            else newBugs

updatePoint' :: Bugs' -> Point' -> Bugs' -> Bugs'
updatePoint' bugs point newBugs =
  let neighbors = countNeighbors' bugs point
   in if Set.member point bugs
        then
          if neighbors /= 1
            then Set.delete point newBugs
            else Set.insert point newBugs
        else
          if neighbors == 1 || neighbors == 2
            then Set.insert point newBugs
            else newBugs

countNeighbors :: Bugs -> Point -> Int
countNeighbors bugs (x, y) = length $ filter (`Set.member` bugs) [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

countNeighbors' :: Bugs' -> Point' -> Int
countNeighbors' bugs (x, y, z) =
  let neighbors = concatMap (recursePoint (x, y, z)) [(x + 1, y, z), (x - 1, y, z), (x, y + 1, z), (x, y - 1, z)]
   in length $ filter (`Set.member` bugs) neighbors

recursePoint :: Point' -> Point' -> [Point']
recursePoint (fx, fy, fz) (x, y, z)
  | x < 0 = [(middle - 1, middle, z + 1)]
  | x > mapSize = [(middle + 1, middle, z + 1)]
  | y < 0 = [(middle, middle - 1, z + 1)]
  | y > mapSize = [(middle, middle + 1, z + 1)]
  | x == middle && y == middle =
      case (fx - middle, fy - middle) of
        (-1, 0) -> map (0,,z - 1) [0 .. mapSize]
        (1, 0) -> map (mapSize,,z - 1) [0 .. mapSize]
        (0, -1) -> map (,0,z - 1) [0 .. mapSize]
        (0, 1) -> map (,mapSize,z - 1) [0 .. mapSize]
  | otherwise = [(x, y, z)]
