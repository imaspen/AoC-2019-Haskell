module Day12.Day12 where

import Data.List (tails)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

type Vec3 = (Int, Int, Int)

type Planet = (Vec3, Vec3)

part1 :: Int -> [[Int]] -> Int
part1 steps startPositions =
  let planets = step steps $ map toPlanet startPositions
   in sum $ map getEnergy planets

part2 :: [[Int]] -> Int
part2 startPositions =
  let planets = map toPlanet startPositions
   in stepUntilRepeat 0 (Nothing, Nothing, Nothing) planets

step :: Int -> [Planet] -> [Planet]
step steps planets
  | steps == 0 = planets
  | otherwise =
      let gravityApplied = map (calculateGravity planets) planets
          moved = map move gravityApplied
          nextSteps = steps - 1
       in step nextSteps moved

stepUntilRepeat :: Int -> (Maybe Int, Maybe Int, Maybe Int) -> [Planet] -> Int
stepUntilRepeat count periods planets = case periods of
  (Just x, Just y, Just z) -> 2 * lcm x (lcm y z)
  _ ->
    let gravityApplied = map (calculateGravity planets) planets
        moved = map move gravityApplied
        nextCount = count + 1
        nextPeriods = updatePeriods nextCount periods moved
     in stepUntilRepeat nextCount nextPeriods moved

updatePeriods :: Int -> (Maybe Int, Maybe Int, Maybe Int) -> [Planet] -> (Maybe Int, Maybe Int, Maybe Int)
updatePeriods count (x, y, z) planets =
  let (xs, ys, zs) = foldr (\(_, (x, y, z)) (xs, ys, zs) -> (xs ++ [x], ys ++ [y], zs ++ [z])) ([], [], []) planets
   in (updatePeriod count x xs, updatePeriod count y ys, updatePeriod count z zs)

updatePeriod :: Int -> Maybe Int -> [Int] -> Maybe Int
updatePeriod count current parts = case current of
  Just _ -> current
  Nothing -> if all (0 ==) parts then Just count else Nothing

calculateGravity :: [Planet] -> Planet -> Planet
calculateGravity planets planet = foldr applyGravity planet planets

applyGravity :: Planet -> Planet -> Planet
applyGravity ((x2, y2, z2), _) (position, (dx, dy, dz)) =
  let (x1, y1, z1) = position
      newVelocity =
        ( if x1 < x2 then dx + 1 else if x1 > x2 then dx - 1 else dx,
          if y1 < y2 then dy + 1 else if y1 > y2 then dy - 1 else dy,
          if z1 < z2 then dz + 1 else if z1 > z2 then dz - 1 else dz
        )
   in (position, newVelocity)

move :: Planet -> Planet
move ((x, y, z), (dx, dy, dz)) = ((x + dx, y + dy, z + dz), (dx, dy, dz))

toPlanet :: [Int] -> Planet
toPlanet ints =
  let position = toVec3 ints
      velocity = (0, 0, 0)
   in (position, velocity)

toVec3 :: [Int] -> Vec3
toVec3 ints = case ints of
  (a : b : c : _) -> (a, b, c)

getEnergy :: Planet -> Int
getEnergy (a, b) = getSubEnergy a * getSubEnergy b

getSubEnergy :: Vec3 -> Int
getSubEnergy (a, b, c) = abs a + abs b + abs c
