module Day6.Day6 where

import qualified Data.Set
import qualified Data.Map
import qualified Data.List.Split

type Planet = String
type Orbits = Data.Set.Set Planet
type Universe = Data.Map.Map Planet Orbits

part1 :: [String] -> Integer
part1 strings = getDepth 0 universe "COM" 
    where
        descriptors = map splitDescriptor strings
        universe = foldr addOrbit emptyUniverse descriptors

splitDescriptor :: String -> (Planet, Planet)
splitDescriptor str = (one, two)
    where 
        splitString = Data.List.Split.splitOn ")" str
        (one : two : []) = splitString

emptyUniverse :: Universe
emptyUniverse = Data.Map.empty

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