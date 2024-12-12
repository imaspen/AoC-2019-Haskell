module Day20.Day20 where

import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vector

type Point = (Int, Int)

-- point, depth
type Position = (Point, Int)

type Portals = Map (Char, Char) [Position]

type PointMap = Vector (Vector Char)

-- Point -> Connected Points
type Nodes = Map Point [Position]

part1 :: [String] -> (Point, Point) -> Int
part1 lines innerBounds =
  let pointMap = Vector.fromList $ map Vector.fromList lines
      height = Vector.length pointMap
      width = Vector.length $ pointMap Vector.! 0
      row = pointMap Vector.! 28
      ((outerMinX, outerMinY), (outerMaxX, outerMaxY)) = ((2, 2), (width - 3, height - 3))
      ((innerMinX, innerMinY), (innerMaxX, innerMaxY)) = innerBounds
      portals1 = getPortalsAbove 0 (getHorizontalSlice innerMinX innerMaxX outerMinY pointMap) (innerMinX, outerMinY) pointMap Map.empty
      portals2 = getPortalsAbove 0 (getHorizontalSlice innerMinX innerMaxX innerMaxY pointMap) (innerMinX, innerMaxY) pointMap portals1
      portals3 = getPortalsBelow 0 (getHorizontalSlice innerMinX innerMaxX innerMinY pointMap) (innerMinX, innerMinY) pointMap portals2
      portals4 = getPortalsBelow 0 (getHorizontalSlice innerMinX innerMaxX outerMaxY pointMap) (innerMinX, outerMaxY) pointMap portals3
      portals5 = getPortalsLeft 0 (getVerticalSlice innerMinY innerMaxY outerMinX pointMap "") (outerMinX, innerMinY) pointMap portals4
      portals6 = getPortalsLeft 0 (getVerticalSlice innerMinY innerMaxY innerMaxX pointMap "") (innerMaxX, innerMinY) pointMap portals5
      portals7 = getPortalsRight 0 (getVerticalSlice innerMinY innerMaxY innerMinX pointMap "") (innerMinX, innerMinY) pointMap portals6
      portals8 = getPortalsRight 0 (getVerticalSlice innerMinY innerMaxY outerMaxX pointMap "") (outerMaxX, innerMinY) pointMap portals7
      ((startPoint, _) : _) = portals8 Map.! ('A', 'A')
      ((endPoint, _) : _) = portals8 Map.! ('Z', 'Z')
      portals = Map.delete ('Z', 'Z') $ Map.delete ('A', 'A') portals8
      nodes = foldr (addNodesInLine pointMap portals) Map.empty (Vector.indexed pointMap)
   in aStar pointMap (startPoint, 0) (endPoint, 0) nodes

part2 :: [String] -> (Point, Point) -> Int
part2 lines innerBounds =
  let pointMap = Vector.fromList $ map Vector.fromList lines
      height = Vector.length pointMap
      width = Vector.length $ pointMap Vector.! 0
      row = pointMap Vector.! 28
      ((outerMinX, outerMinY), (outerMaxX, outerMaxY)) = ((2, 2), (width - 3, height - 3))
      ((innerMinX, innerMinY), (innerMaxX, innerMaxY)) = innerBounds
      portals1 = getPortalsAbove 1 (getHorizontalSlice innerMinX innerMaxX outerMinY pointMap) (innerMinX, outerMinY) pointMap Map.empty
      portals2 = getPortalsAbove (-1) (getHorizontalSlice innerMinX innerMaxX innerMaxY pointMap) (innerMinX, innerMaxY) pointMap portals1
      portals3 = getPortalsBelow (-1) (getHorizontalSlice innerMinX innerMaxX innerMinY pointMap) (innerMinX, innerMinY) pointMap portals2
      portals4 = getPortalsBelow 1 (getHorizontalSlice innerMinX innerMaxX outerMaxY pointMap) (innerMinX, outerMaxY) pointMap portals3
      portals5 = getPortalsLeft 1 (getVerticalSlice innerMinY innerMaxY outerMinX pointMap "") (outerMinX, innerMinY) pointMap portals4
      portals6 = getPortalsLeft (-1) (getVerticalSlice innerMinY innerMaxY innerMaxX pointMap "") (innerMaxX, innerMinY) pointMap portals5
      portals7 = getPortalsRight (-1) (getVerticalSlice innerMinY innerMaxY innerMinX pointMap "") (innerMinX, innerMinY) pointMap portals6
      portals8 = getPortalsRight 1 (getVerticalSlice innerMinY innerMaxY outerMaxX pointMap "") (outerMaxX, innerMinY) pointMap portals7
      ((startPoint, _) : _) = portals8 Map.! ('A', 'A')
      ((endPoint, _) : _) = portals8 Map.! ('Z', 'Z')
      portals = Map.delete ('Z', 'Z') $ Map.delete ('A', 'A') portals8
      nodes = foldr (addNodesInLine pointMap portals) Map.empty (Vector.indexed pointMap)
   in dijkstra pointMap (startPoint, 0) (endPoint, 0) nodes

getHorizontalSlice :: Int -> Int -> Int -> PointMap -> String
getHorizontalSlice minX maxX y pointMap = Vector.toList $ Vector.slice minX (maxX - minX) $ pointMap Vector.! y

getVerticalSlice :: Int -> Int -> Int -> PointMap -> String -> String
getVerticalSlice minY maxY x pointMap acc = case maxY - minY of
  -1 -> reverse acc
  _ -> getVerticalSlice (minY + 1) maxY x pointMap (getAt (x, minY) pointMap : acc)

getPortalsAbove :: Int -> String -> Point -> PointMap -> Portals -> Portals
getPortalsAbove depthOffset row (x, y) map portals = case row of
  (c : rest) ->
    let nextPortals = case c of
          '.' -> Map.insertWith (++) (getAt (x, y - 2) map, getAt (x, y - 1) map) [((x, y), depthOffset)] portals
          _ -> portals
     in getPortalsAbove depthOffset rest (x + 1, y) map nextPortals
  [] -> portals

getPortalsBelow :: Int -> String -> Point -> PointMap -> Portals -> Portals
getPortalsBelow depthOffset row (x, y) map portals = case row of
  (c : rest) ->
    let nextPortals = case c of
          '.' -> Map.insertWith (++) (getAt (x, y + 1) map, getAt (x, y + 2) map) [((x, y), depthOffset)] portals
          _ -> portals
     in getPortalsBelow depthOffset rest (x + 1, y) map nextPortals
  [] -> portals

getPortalsLeft :: Int -> String -> Point -> PointMap -> Portals -> Portals
getPortalsLeft depthOffset row (x, y) map portals = case row of
  (c : rest) ->
    let nextPortals = case c of
          '.' -> Map.insertWith (++) (getAt (x - 2, y) map, getAt (x - 1, y) map) [((x, y), depthOffset)] portals
          _ -> portals
     in getPortalsLeft depthOffset rest (x, y + 1) map nextPortals
  [] -> portals

getPortalsRight :: Int -> String -> Point -> PointMap -> Portals -> Portals
getPortalsRight depthOffset row (x, y) map portals = case row of
  (c : rest) ->
    let nextPortals = case c of
          '.' -> Map.insertWith (++) (getAt (x + 1, y) map, getAt (x + 2, y) map) [((x, y), depthOffset)] portals
          _ -> portals
     in getPortalsRight depthOffset rest (x, y + 1) map nextPortals
  [] -> portals

getAt :: Point -> PointMap -> Char
getAt (x, y) pointMap = pointMap Vector.! y Vector.! x

addNodesInLine :: PointMap -> Portals -> (Int, Vector Char) -> Nodes -> Nodes
addNodesInLine pointMap portals (y, row) nodes = foldr (addNode pointMap portals y) nodes (Vector.indexed row)

addNode :: PointMap -> Portals -> Int -> (Int, Char) -> Nodes -> Nodes
addNode pointMap portals y (x, char) nodes = case char of
  '.' -> Map.insert (x, y) (getNeighborPortal (x, y) portals ++ getNeighbors pointMap (x, y)) nodes
  _ -> nodes

getNeighborPortal :: Point -> Portals -> [Position]
getNeighborPortal point portals =
  case List.find (Maybe.isJust . List.find (\(p, _) -> p == point)) $ Map.elems portals of
    Nothing -> []
    Just ((p1 : p2 : _)) ->
      let (point1, _) = p1
       in if point1 == point then [p2] else [p1]

getNeighbors :: PointMap -> Point -> [Position]
getNeighbors pointMap (x, y) = filter (\(p, _) -> '.' == getAt p pointMap) [((x + 1, y), 0), ((x - 1, y), 0), ((x, y + 1), 0), ((x, y - 1), 0)]

-- A*

type OpenSet = Set Position

type CameFrom = Map Position Position

type FScores = Map Position Int

type GScores = Map Position Int

bigNumber :: Int
bigNumber = maxBound

h :: Position -> Position -> Int
h ((endX, endY), _) ((x, y), level) = abs (endX - x) + abs (endY - y)

aStar :: PointMap -> Position -> Position -> Nodes -> Int
aStar pointMap start end nodes =
  let openSet = Set.singleton start
      gScores = Map.singleton start 0
      fScores = Map.singleton start $ h end start
      cameFrom = aStarLoop pointMap end nodes openSet Map.empty fScores gScores
   in unfold start cameFrom end 0

aStarLoop :: PointMap -> Position -> Nodes -> OpenSet -> CameFrom -> FScores -> GScores -> CameFrom
aStarLoop pointMap end nodes openSet cameFrom fScores gScores =
  let current = List.minimumBy (\a b -> Map.findWithDefault bigNumber a fScores `compare` Map.findWithDefault bigNumber b fScores) $ Set.toList openSet
   in if current == end
        then cameFrom
        else
          let openWithoutCurrent = Set.delete current openSet
              (currentPos, currentLevel) = current
              neighbors = Maybe.mapMaybe (offsetNeighbor current) $ nodes Map.! currentPos
              (nextOpenSet, nextCameFrom, nextFScores, nextGScores) = foldr (updateNeighbor end current) (openWithoutCurrent, cameFrom, fScores, gScores) neighbors
           in aStarLoop pointMap end nodes nextOpenSet nextCameFrom nextFScores nextGScores

offsetNeighbor :: Position -> Position -> Maybe Position
offsetNeighbor (_, level) (pos, offset) =
  let newLevel = level + offset
   in if newLevel < 0 then Nothing else Just (pos, newLevel)

updateNeighbor :: Position -> Position -> Position -> (OpenSet, CameFrom, FScores, GScores) -> (OpenSet, CameFrom, FScores, GScores)
updateNeighbor target current neighbor (openSet, cameFrom, fScores, gScores) =
  let (_, currentLevel) = current
      (_, neighborLevel) = current
      nextGScore = (gScores Map.! current) + 1
   in if nextGScore >= Map.findWithDefault bigNumber neighbor gScores
        then (openSet, cameFrom, fScores, gScores)
        else
          ( Set.insert neighbor openSet,
            Map.insert neighbor current cameFrom,
            Map.insert neighbor (nextGScore + h target neighbor) fScores,
            Map.insert neighbor nextGScore gScores
          )

unfold :: Position -> CameFrom -> Position -> Int -> Int
unfold target cameFrom at acc =
  if target == at
    then acc
    else unfold target cameFrom (cameFrom Map.! at) (acc + 1)

-- dijkstra

type Distances = Map Position Int

type Queue = Set Position

dijkstra :: PointMap -> Position -> Position -> Nodes -> Int
dijkstra pointMap start end nodes =
  let queue = Set.singleton start
      distances = Map.singleton start 0
   in dijkstraLoop pointMap end nodes queue distances Map.! end

dijkstraLoop :: PointMap -> Position -> Nodes -> Queue -> Distances -> Distances
dijkstraLoop pointMap end nodes queue distances =
  let current = List.minimumBy (\(aPos, aLevel) (bPos, bLevel) -> if aLevel /= bLevel then compare aLevel bLevel else compare (Map.findWithDefault maxBound (aPos, aLevel) distances) (Map.findWithDefault maxBound (bPos, bLevel) distances)) queue
      queueWithoutCurrent = Set.delete current queue
   in if current == end
        then distances
        else
          let (currentPos, _) = current
              neighbors = Maybe.mapMaybe (offsetNeighbor current) $ nodes Map.! currentPos
              (newQueue, newDistances) = foldr (updateNeighborDijkstra current) (queueWithoutCurrent, distances) neighbors
           in dijkstraLoop pointMap end nodes newQueue newDistances

updateNeighborDijkstra :: Position -> Position -> (Queue, Distances) -> (Queue, Distances)
updateNeighborDijkstra current neighbor (queue, distances) =
  let tentativeDistance = 1 + distances Map.! current
   in if tentativeDistance >= Map.findWithDefault maxBound neighbor distances
        then (queue, distances)
        else (Set.insert neighbor queue, Map.insert neighbor tentativeDistance distances)
