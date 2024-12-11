module Day20.Day20 where

import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Debug.Trace (traceShow)

type Point = (Int, Int)

type Portals = Map (Char, Char) [Point]

type PointMap = Vector (Vector Char)

-- Point -> Connected Points
type Nodes = Map Point [Point]

part1 :: [String] -> (Point, Point) -> Int
part1 lines innerBounds =
  let pointMap = Vector.fromList $ map Vector.fromList lines
      height = Vector.length pointMap
      width = Vector.length $ pointMap Vector.! 0
      row = pointMap Vector.! 28
      ((outerMinX, outerMinY), (outerMaxX, outerMaxY)) = ((2, 2), (width - 3, height - 3))
      ((innerMinX, innerMinY), (innerMaxX, innerMaxY)) = innerBounds
      portals1 = getPortalsAbove (getHorizontalSlice innerMinX innerMaxX outerMinY pointMap) (innerMinX, outerMinY) pointMap Map.empty
      portals2 = getPortalsAbove (getHorizontalSlice innerMinX innerMaxX innerMaxY pointMap) (innerMinX, innerMaxY) pointMap portals1
      portals3 = getPortalsBelow (getHorizontalSlice innerMinX innerMaxX innerMinY pointMap) (innerMinX, innerMinY) pointMap portals2
      portals4 = getPortalsBelow (getHorizontalSlice innerMinX innerMaxX outerMaxY pointMap) (innerMinX, outerMaxY) pointMap portals3
      portals5 = getPortalsLeft (getVerticalSlice innerMinY innerMaxY outerMinX pointMap "") (outerMinX, innerMinY) pointMap portals4
      portals6 = getPortalsLeft (getVerticalSlice innerMinY innerMaxY innerMaxX pointMap "") (innerMaxX, innerMinY) pointMap portals5
      portals7 = getPortalsRight (getVerticalSlice innerMinY innerMaxY innerMinX pointMap "") (innerMinX, innerMinY) pointMap portals6
      portals8 = getPortalsRight (getVerticalSlice innerMinY innerMaxY outerMaxX pointMap "") (outerMaxX, innerMinY) pointMap portals7
      (startPoint : _) = portals8 Map.! ('A', 'A')
      (endPoint : _) = portals8 Map.! ('Z', 'Z')
      portals = Map.delete ('Z', 'Z') $ Map.delete ('A', 'A') portals8
      nodes = foldr (addNodesInLine pointMap portals) Map.empty (Vector.indexed pointMap)
   in aStar pointMap startPoint endPoint nodes

part2 :: [String] -> Int
part2 lines = 0

getHorizontalSlice :: Int -> Int -> Int -> PointMap -> String
getHorizontalSlice minX maxX y pointMap = Vector.toList $ Vector.slice minX (maxX - minX) $ pointMap Vector.! y

getVerticalSlice :: Int -> Int -> Int -> PointMap -> String -> String
getVerticalSlice minY maxY x pointMap acc = case maxY - minY of
  -1 -> reverse acc
  _ -> getVerticalSlice (minY + 1) maxY x pointMap (getAt (x, minY) pointMap : acc)

getPortalsAbove :: String -> Point -> PointMap -> Portals -> Portals
getPortalsAbove row (x, y) map portals = case row of
  (c : rest) ->
    let nextPortals = case c of
          '.' -> Map.insertWith (++) (getAt (x, y - 2) map, getAt (x, y - 1) map) [(x, y)] portals
          _ -> portals
     in getPortalsAbove rest (x + 1, y) map nextPortals
  [] -> portals

getPortalsBelow :: String -> Point -> PointMap -> Portals -> Portals
getPortalsBelow row (x, y) map portals = case row of
  (c : rest) ->
    let nextPortals = case c of
          '.' -> Map.insertWith (++) (getAt (x, y + 1) map, getAt (x, y + 2) map) [(x, y)] portals
          _ -> portals
     in getPortalsBelow rest (x + 1, y) map nextPortals
  [] -> portals

getPortalsLeft :: String -> Point -> PointMap -> Portals -> Portals
getPortalsLeft row (x, y) map portals = case row of
  (c : rest) ->
    let nextPortals = case c of
          '.' -> Map.insertWith (++) (getAt (x - 2, y) map, getAt (x - 1, y) map) [(x, y)] portals
          _ -> portals
     in getPortalsLeft rest (x, y + 1) map nextPortals
  [] -> portals

getPortalsRight :: String -> Point -> PointMap -> Portals -> Portals
getPortalsRight row (x, y) map portals = case row of
  (c : rest) ->
    let nextPortals = case c of
          '.' -> Map.insertWith (++) (getAt (x + 1, y) map, getAt (x + 2, y) map) [(x, y)] portals
          _ -> portals
     in getPortalsRight rest (x, y + 1) map nextPortals
  [] -> portals

getAt :: Point -> PointMap -> Char
getAt (x, y) pointMap = pointMap Vector.! y Vector.! x

addNodesInLine :: PointMap -> Portals -> (Int, Vector Char) -> Nodes -> Nodes
addNodesInLine pointMap portals (y, row) nodes = foldr (addNode pointMap portals y) nodes (Vector.indexed row)

addNode :: PointMap -> Portals -> Int -> (Int, Char) -> Nodes -> Nodes
addNode pointMap portals y (x, char) nodes = case char of
  '.' -> Map.insert (x, y) (getNeighborPortal (x, y) portals ++ getNeighbors pointMap (x, y)) nodes
  _ -> nodes

getNeighborPortal :: Point -> Portals -> [Point]
getNeighborPortal point portals =
  case List.find (elem point) $ Map.elems portals of
    Nothing -> []
    Just ((p1 : p2 : _)) -> if p1 == point then [p2] else [p1]

getNeighbors :: PointMap -> Point -> [Point]
getNeighbors pointMap (x, y) = filter (\p -> '.' == getAt p pointMap) [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

-- A*

type OpenSet = Set Point

type CameFrom = Map Point Point

type FScores = Map Point Int

type GScores = Map Point Int

bigNumber :: Int
bigNumber = 10000000

h :: Point -> Point -> Int
h (endX, endY) (x, y) = abs (endX - x) + abs (endY - y)

aStar :: PointMap -> Point -> Point -> Nodes -> Int
aStar pointMap start end nodes =
  let openSet = Set.singleton start
      gScores = Map.singleton start 0
      fScores = Map.singleton start $ h end start
      cameFrom = aStarLoop pointMap end nodes openSet Map.empty fScores gScores
   in unfold start cameFrom end 0

aStarLoop :: PointMap -> Point -> Nodes -> OpenSet -> CameFrom -> FScores -> GScores -> CameFrom
aStarLoop pointMap end nodes openSet cameFrom fScores gScores =
  let current = List.minimumBy (\a b -> Map.findWithDefault bigNumber a fScores `compare` Map.findWithDefault bigNumber b fScores) $ Set.toList openSet
   in if current == end
        then cameFrom
        else
          let openWithoutCurrent = Set.delete current openSet
              neighbors = nodes Map.! current
              (nextOpenSet, nextCameFrom, nextFScores, nextGScores) = foldr (updateNeighbor end current) (openWithoutCurrent, cameFrom, fScores, gScores) neighbors
           in aStarLoop pointMap end nodes nextOpenSet nextCameFrom nextFScores nextGScores

updateNeighbor :: Point -> Point -> Point -> (OpenSet, CameFrom, FScores, GScores) -> (OpenSet, CameFrom, FScores, GScores)
updateNeighbor target current neighbor (openSet, cameFrom, fScores, gScores) =
  let nextGScore = (gScores Map.! current) + 1
   in if nextGScore >= Map.findWithDefault bigNumber neighbor gScores
        then (openSet, cameFrom, fScores, gScores)
        else
          ( Set.insert neighbor openSet,
            Map.insert neighbor current cameFrom,
            Map.insert neighbor (nextGScore + h target neighbor) fScores,
            Map.insert neighbor nextGScore gScores
          )

unfold :: Point -> CameFrom -> Point -> Int -> Int
unfold target cameFrom at acc =
  if target == at
    then acc
    else unfold target cameFrom (cameFrom Map.! at) (acc + 1)
