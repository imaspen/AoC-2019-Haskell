module Day11.Day11 where

import Data.Ix (Ix (range))
import Data.Set (Set)
import qualified Data.Set as Set

type Memory = [Int]

type Address = Int

type RelativeBase = Int

type Input = [Int]

type Output = [Int]

type Halted = Bool

type Point = (Int, Int)

data Direction = North | South | East | West deriving (Eq, Ord, Enum, Show)

type Painted = Set Point

type State = (Memory, Address, RelativeBase, Input, Output, Halted, Point, Direction, Painted, Painted)

part1 :: Memory -> Int
part1 memory = Set.size everPainted
  where
    (_, _, _, _, _, _, _, _, _, everPainted) = step (memory ++ repeat 0, 0, 0, [], [], False, (0, 0), North, Set.empty, Set.empty)

part2 :: Memory -> String
part2 memory = createMap painted
  where
    (_, _, _, _, _, _, _, _, painted, _) = step (memory ++ repeat 0, 0, 0, [2], [], False, (0, 0), North, Set.singleton (0, 0), Set.empty)

createMap :: Painted -> String
createMap painted = concatMap (createRow painted) yRange
  where
    ys = Set.map snd painted
    yRange = range (Set.findMin ys, Set.findMax ys)

createRow :: Painted -> Int -> String
createRow painted y = map (\x -> if Set.member (x, y) painted then '#' else ' ') xRange ++ ['\n']
  where
    xs = Set.map fst painted
    xRange = range (Set.findMin xs, Set.findMax xs)

step :: State -> State
step state
  | halted = state
  | otherwise = step $ execute state
  where
    (_, _, _, _, _, halted, _, _, _, _) = state

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
  | op == 99 = (memory, address, rel, input, output, True, position, direction, painted, everPainted)
  where
    (memory, address, rel, input, output, halted, position, direction, painted, everPainted) = state
    op = opcode $ getAt memory address

arithmetic :: (Int -> Int -> Int) -> State -> State
arithmetic op state = (newMem, address + 4, r, i, o, h, position, direction, painted, everPainted)
  where
    (memory, address, r, i, o, h, position, direction, painted, everPainted) = state
    v1 = getWithParam 0 state
    v2 = getWithParam 1 state
    pos = getPos 2 state
    newMem = insert memory pos (v1 `op` v2)

getInput :: State -> State
getInput state = (newMem, address + 2, r, input, o, h, position, direction, painted, everPainted)
  where
    (memory, address, r, input, o, h, position, direction, painted, everPainted) = state
    val = if Set.member position painted then 1 else 0
    pos = getPos 0 state
    newMem = insert memory pos val

putOutput :: State -> State
putOutput state = case newOut of
  (turnInstruction : paintInstruction : _) ->
    let newDirection = turn direction turnInstruction
        newPosition = move position newDirection
        (newPainted, newEverPainted) = paint painted everPainted position paintInstruction
     in (memory, address + 2, r, i, [], h, newPosition, newDirection, newPainted, newEverPainted)
  _ -> (memory, address + 2, r, i, newOut, h, position, direction, painted, everPainted)
  where
    (memory, address, r, i, output, h, position, direction, painted, everPainted) = state
    val = getWithParam 0 state
    newOut = val : output

turn :: Direction -> Int -> Direction
turn direction input
  | direction == North && input == 0 = West
  | direction == North && input == 1 = East
  | direction == South && input == 0 = East
  | direction == South && input == 1 = West
  | direction == East && input == 0 = North
  | direction == East && input == 1 = South
  | direction == West && input == 0 = South
  | direction == West && input == 1 = North

move :: Point -> Direction -> Point
move (x, y) direction = case direction of
  North -> (x, y - 1)
  South -> (x, y + 1)
  East -> (x + 1, y)
  West -> (x - 1, y)

paint :: Painted -> Painted -> Point -> Int -> (Painted, Painted)
paint painted everPainted position input
  | input == 0 = (Set.delete position painted, Set.insert position everPainted)
  | input == 1 = (Set.insert position painted, Set.insert position everPainted)

jump :: (Int -> Bool) -> State -> State
jump test state
  | test v1 = (memory, v2, r, i, o, h, position, direction, painted, everPainted)
  | otherwise = (memory, address + 3, r, i, o, h, position, direction, painted, everPainted)
  where
    (memory, address, r, i, o, h, position, direction, painted, everPainted) = state
    v1 = getWithParam 0 state
    v2 = getWithParam 1 state

comp :: (Int -> Int -> Bool) -> State -> State
comp op state
  | v1 `op` v2 = (newMem 1, address + 4, r, i, o, h, position, direction, painted, everPainted)
  | otherwise = (newMem 0, address + 4, r, i, o, h, position, direction, painted, everPainted)
  where
    (memory, address, r, i, o, h, position, direction, painted, everPainted) = state
    v1 = getWithParam 0 state
    v2 = getWithParam 1 state
    pos = getPos 2 state
    newMem = insert memory pos

offsetRel :: State -> State
offsetRel state = (memory, address + 2, newRel, i, o, h, position, direction, painted, everPainted)
  where
    (memory, address, rel, i, o, h, position, direction, painted, everPainted) = state
    ps = getParams memory address
    v1 = getWithParam 0 state
    newRel = rel + v1

insert :: Memory -> Int -> Int -> Memory
insert memory pos val = before ++ [val] ++ after
  where
    (before, _ : after) = splitAt pos memory

getWithParam :: Int -> State -> Int
getWithParam i (m, a, r, _, _, _, _, _, _, _) = get r (parameterMode i ps) m (a + i + 1)
  where
    ps = getParams m a

getPos :: Int -> State -> Int
getPos offset (m, a, r, _, _, _, _, _, _, _)
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
getRel offset memory address = getAt memory ((getAt memory address) + offset)

getParams :: Memory -> Address -> Int
getParams m a = parameters $ getAt m a

opcode :: Int -> Int
opcode instruction = instruction `rem` 100

parameters :: Int -> Int
parameters instruction = instruction `quot` 100

parameterMode :: Int -> Int -> Int
parameterMode position instruction = (instruction `quot` (10 ^ position)) `rem` 10
