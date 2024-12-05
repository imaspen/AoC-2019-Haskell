module Day15.Day15 where

import Data.Foldable (find, minimumBy)
import Data.Function (on)
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace (traceShow)

-- import Debug.Trace (traceShowId)
traceShowId :: a -> a
traceShowId a = a

type Memory = [Int]

type Address = Int

type RelativeBase = Int

type Input = (Direction, Position, Seen)

type Output = [Int]

type Halted = Bool

type State = (Memory, Address, RelativeBase, Input, Output, Halted)

data Direction = North | South | West | East deriving (Eq, Show)

toDirection :: Int -> Direction
toDirection 1 = North
toDirection 2 = South
toDirection 3 = West
toDirection 4 = East

fromDirection :: Direction -> Int
fromDirection North = 1
fromDirection South = 2
fromDirection West = 3
fromDirection East = 4

turnRight :: Direction -> Direction
turnRight North = East
turnRight East = South
turnRight South = West
turnRight West = North

turnLeft :: Direction -> Direction
turnLeft North = West
turnLeft West = South
turnLeft South = East
turnLeft East = North

data Location = Wall | Empty | Goal deriving (Eq, Show)

toLocation :: Int -> Location
toLocation 0 = Wall
toLocation 1 = Empty
toLocation 2 = Goal

fromLocation :: Location -> Int
fromLocation Wall = 0
fromLocation Empty = 1
fromLocation Goal = 2

type ToCheck = (Direction, Input)

type Position = (Int, Int)

move :: Direction -> Position -> Position
move direction (x, y) = case direction of
  North -> (x + 1, y)
  South -> (x - 1, y)
  East -> (x, y + 1)
  West -> (x, y - 1)

type Seen = Map Position Location

data Distance = Dist Int | Infinity deriving (Eq, Show)

instance Ord Distance where
  Infinity <= Infinity = True
  Infinity <= Dist _ = False
  Dist _ <= Infinity = True
  Dist x <= Dist y = x <= y

addDistance :: Distance -> Distance -> Distance
addDistance (Dist a) (Dist b) = Dist (a + b)
addDistance _ _ = Infinity

type Distances = Map Position Distance

getDistance :: Distances -> Position -> Distance
getDistance distances position = Maybe.fromMaybe Infinity (Map.lookup position distances)

type Previous = Map Position Position

type Queue = Set Position

part1 :: Memory -> String
part1 memory =
  let input = (North, (0, 0), Map.singleton (0, 0) Empty)
      output = step (memory ++ repeat 0, 0, 0, input, [], False)
      (_, _, _, (_, _, seen), _, _) = output
      cleanSeen = Map.filter (/= Wall) seen
      queue = Map.keysSet cleanSeen
      dist = Map.singleton (0, 0) (Dist 0)
      previous = Map.empty
      (distances, _) = loop queue dist previous
      (goal, _) = Maybe.fromMaybe ((0, 0), Goal) $ find (\(_, l) -> l == Goal) $ Map.toList cleanSeen
   in show $ distances Map.! goal

part2 :: Memory -> String
part2 memory =
  let input = (North, (0, 0), Map.singleton (0, 0) Empty)
      output = step (memory ++ repeat 0, 0, 0, input, [], False)
      (_, _, _, (_, _, seen), _, _) = output
      cleanSeen = Map.filter (/= Wall) seen
      (goal, _) = Maybe.fromMaybe ((0, 0), Goal) $ find (\(_, l) -> l == Goal) $ Map.toList cleanSeen
      queue = Map.keysSet cleanSeen
      dist = Map.singleton goal (Dist 0)
      previous = Map.empty
      (distances, _) = loop queue dist previous
   in show $ maximum (Map.elems distances)

loop :: Queue -> Distances -> Previous -> (Distances, Previous)
loop queue distances previous =
  if Set.null queue
    then (distances, previous)
    else
      let u = minimumBy (compare `on` getDistance distances) queue
          newQueue = Set.delete u queue
          dist = addDistance (Dist 1) $ distances Map.! u
          ns = neighbors queue u
          (newDistances, newPrevious) = List.foldr (updateDistance dist u) (distances, previous) ns
       in loop newQueue newDistances newPrevious

neighbors :: Queue -> Position -> [Position]
neighbors queue pos =
  let neighbors = [move North pos, move East pos, move South pos, move West pos]
   in List.filter (`Set.member` queue) neighbors

updateDistance :: Distance -> Position -> Position -> (Distances, Previous) -> (Distances, Previous)
updateDistance dist from pos (distances, previous) =
  let currentDist = getDistance distances pos
      newDist = min dist currentDist
      newPrev = if traceShow (dist, currentDist) (dist < currentDist) then Map.insert pos from previous else previous
   in (Map.insert pos newDist distances, previous)

-- part2 :: Memory -> Output
-- part2 memory = output
--   where
--     (_, _, _, _, output, _) = step (memory ++ repeat 0, 0, 0, [2], [], False)

-------- INT-CODE --------

step :: State -> State
step state
  | halted = state
  | otherwise = step $ execute state
  where
    (_, _, _, _, _, halted) = state

execute :: State -> State
execute state
  | op == 1 = arithmetic (+) state
  | op == 2 = arithmetic (*) state
  | op == 3 = getInput state
  | op == 4 = putOutput state
  | op == 5 = jump (/= 0) state
  | op == 6 = jump (== 0) state
  | op == 7 = comp (<) state
  | op == 8 = comp (==) state
  | op == 9 = offsetRel state
  | op == 99 = (memory, address, rel, input, output, True)
  where
    (memory, address, rel, input, output, halted) = state
    op = opcode $ getAt memory address

arithmetic :: (Int -> Int -> Int) -> State -> State
arithmetic op state = (newMem, address + 4, r, i, o, h)
  where
    (memory, address, r, i, o, h) = state
    v1 = getWithParam 0 state
    v2 = getWithParam 1 state
    pos = getPos 2 state
    newMem = insert memory pos (v1 `op` v2)

getInput :: State -> State
getInput state = (newMem, address + 2, r, input, o, h)
  where
    (memory, address, r, input, o, h) = state
    (direction, _, _) = input
    val = fromDirection direction
    pos = getPos 0 state
    newMem = insert memory pos val

putOutput :: State -> State
putOutput state = (memory, address + 2, r, newInput, newOut, halt)
  where
    (memory, address, r, input, output, h) = state
    val = getWithParam 0 state
    newOut = val : output
    newInput = updateInput (toLocation val) input
    (_, (x, y), _) = input
    (_, (nx, ny), _) = newInput
    halt = not (x == 0 && y == 0) && (nx == 0 && ny == 0)

updateInput :: Location -> Input -> Input
updateInput Wall (direction, position, seen) =
  let newDirection = turnRight direction
      newSeen = Map.insert (move direction position) Wall seen
   in (newDirection, position, newSeen)
updateInput location (direction, position, seen) =
  let newDirection = turnLeft direction
      newPosition = move direction position
      newSeen = Map.insert newPosition location seen
   in (newDirection, newPosition, newSeen)

jump :: (Int -> Bool) -> State -> State
jump test state
  | test v1 = (memory, v2, r, i, o, h)
  | otherwise = (memory, address + 3, r, i, o, h)
  where
    (memory, address, r, i, o, h) = state
    v1 = getWithParam 0 state
    v2 = getWithParam 1 state

comp :: (Int -> Int -> Bool) -> State -> State
comp op state
  | v1 `op` v2 = (newMem 1, address + 4, r, i, o, h)
  | otherwise = (newMem 0, address + 4, r, i, o, h)
  where
    (memory, address, r, i, o, h) = state
    v1 = getWithParam 0 state
    v2 = getWithParam 1 state
    pos = getPos 2 state
    newMem = insert memory pos

offsetRel :: State -> State
offsetRel state = (memory, address + 2, newRel, i, o, h)
  where
    (memory, address, rel, i, o, h) = state
    ps = getParams memory address
    v1 = getWithParam 0 state
    newRel = rel + v1

insert :: Memory -> Int -> Int -> Memory
insert memory pos val = before ++ [val] ++ after
  where
    (before, _ : after) = splitAt pos memory

getWithParam :: Int -> State -> Int
getWithParam i (m, a, r, _, _, _) = get r (parameterMode i ps) m (a + i + 1)
  where
    ps = getParams m a

getPos :: Int -> State -> Int
getPos offset (m, a, r, _, _, _)
  | mode == 0 = val
  | mode == 2 = val + r
  where
    ps = parameters $ getAt m a
    mode = parameterMode offset ps
    val = getAt m (a + offset + 1)

get :: Int -> Int -> Memory -> Address -> Int
get rel mode
  | mode == 0 = getValue
  | mode == 1 = getAt
  | mode == 2 = getRel rel

getAt :: Memory -> Address -> Int
getAt memory = (memory !!)

getValue :: Memory -> Address -> Int
getValue memory address = getAt memory $ getAt memory address

getRel :: Int -> Memory -> Address -> Int
getRel offset memory address = getAt memory (getAt memory address + offset)

getParams :: Memory -> Address -> Int
getParams m a = parameters $ getAt m a

opcode :: Int -> Int
opcode instruction = instruction `rem` 100

parameters :: Int -> Int
parameters instruction = instruction `quot` 100

parameterMode :: Int -> Int -> Int
parameterMode position instruction = instruction `quot` (10 ^ position) `rem` 10
