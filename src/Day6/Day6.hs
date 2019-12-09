module Day6.Day6 where

import qualified Data.Set
import qualified Data.Map
import qualified Data.List.Split

type Planet = String
type Orbits = Data.Set.Set Planet
type Universe = Data.Map.Map Planet Orbits

type BacktrackMap = Data.Map.Map Planet Planet

part1 :: [String] -> Integer
part1 strings = getDepth 0 universe "COM" 
    where
        descriptors = map splitDescriptor strings
        universe = foldr addOrbit Data.Map.empty descriptors

part2 :: [String] -> Integer
part2 strings = santaDist + youDist
    where
        descriptors = map splitDescriptor strings
        backtrackMap = foldr addBacktrack Data.Map.empty descriptors
        santaMap = backtrackFrom "COM" "SAN" backtrackMap
        youMap = backtrackFrom "COM" "YOU" backtrackMap
        overlap = last $ overlaps (reverse santaMap) (reverse youMap)
        santaDist = distance (tail santaMap) overlap
        youDist = distance (tail youMap) overlap

splitDescriptor :: String -> (Planet, Planet)
splitDescriptor str = (one, two)
    where 
        splitString = Data.List.Split.splitOn ")" str
        (one : two : []) = splitString

addOrbit :: (Planet, Planet) -> Universe -> Universe
addOrbit (p1, p2) u
    | Data.Map.member p1 u = Data.Map.adjust (addPlanet p2) p1 u
    | otherwise = Data.Map.insert p1 (Data.Set.singleton p2) u

addPlanet :: Planet -> Data.Set.Set Planet -> Data.Set.Set Planet
addPlanet p ps = Data.Set.insert p ps

getDepth :: Integer -> Universe -> Planet -> Integer
getDepth i u p = Data.Set.foldr fun i ps
    where 
        ps = Data.Map.findWithDefault Data.Set.empty p u
        fun = addDepth (i + 1) u

addDepth :: Integer -> Universe -> Planet -> Integer -> Integer
addDepth i1 u p i2 = i2 + getDepth i1 u p

addBacktrack :: (Planet, Planet) -> BacktrackMap -> BacktrackMap
addBacktrack (p1, p2) = Data.Map.insert p2 p1

backtrackFrom :: Planet -> Planet -> BacktrackMap -> [Planet]
backtrackFrom t p b 
    | p == t = [p]
    | otherwise = p : (backtrackFrom t p2 b)
    where p2 = Data.Map.findWithDefault "" p b

overlaps :: [Planet] -> [Planet] -> [Planet]
overlaps (a : as) (b : bs)
    | a == b = a : overlaps as bs
    | otherwise = []

distance :: [Planet] -> Planet -> Integer
distance (p:ps) t
    | p == t = 0
    | otherwise = 1 + (distance ps t)