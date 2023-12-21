module Day13.Day13 where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

type Memory = [Int]

type Address = Int

type RelativeBase = Int

type Input = [Int]

type Output = [Int]

type Halted = Bool

type Point = (Int, Int)

type State = (Memory, Address, RelativeBase, Input, Output, Halted, Output, Map Point Int)

part1 :: Memory -> Int
part1 memory = countBricks output
  where
    (_, _, _, _, output, _, _, _) = step (memory ++ repeat 0, 0, 0, [], [], False, [], Map.empty)

countBricks :: Output -> Int
countBricks (2 : _ : _ : rest) = 1 + countBricks rest
countBricks (_ : _ : _ : rest) = countBricks rest
countBricks _ = 0

part2 :: Memory -> Int
part2 memory = tiles Map.! (-1, 0)
  where
    (_, _, _, _, _, _, _, tiles) = step ([2] ++ drop 1 memory ++ repeat 0, 0, 0, [], [], False, [], Map.empty)

step :: State -> State
step state
  | halted = state
  | otherwise = step $ execute state
  where
    (_, _, _, _, _, halted, _, _) = state

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
  | op == 99 = (memory, address, rel, input, output, True, nextTile, tiles)
  where
    (memory, address, rel, input, output, halted, nextTile, tiles) = state
    op = opcode $ getAt memory address

arithmetic :: (Int -> Int -> Int) -> State -> State
arithmetic op state = (newMem, address + 4, r, i, o, h, n, t)
  where
    (memory, address, r, i, o, h, n, t) = state
    v1 = getWithParam 0 state
    v2 = getWithParam 1 state
    pos = getPos 2 state
    newMem = insert memory pos (v1 `op` v2)

getInput :: State -> State
getInput state = (newMem, address + 2, r, newInput, o, h, n, t)
  where
    (memory, address, r, input, o, h, n, t) = state
    (val, newInput) = getInputVal input
    pos = getPos 0 state
    newMem = insert memory pos val

getInputVal :: Input -> (Int, Input)
getInputVal (val : rest) = (val, rest)
-- don't bother moving the paddle, just add blocks to the bottom of the screen ;)
getInputVal _ = (0, [])

putOutput :: State -> State
putOutput state =
  let (memory, address, r, i, output, h, nextTile, tiles) = state
      val = getWithParam 0 state
      newOut = val : output
      (newTile, newTiles, wasReplacement) = updateTiles val nextTile tiles
      halted = wasReplacement && Map.null (Map.filter (2 ==) tiles)
   in (memory, address + 2, r, i, newOut, halted, newTile, newTiles)

updateTiles :: Int -> [Int] -> Map Point Int -> ([Int], Map Point Int, Bool)
updateTiles id (y : x : _) tiles = ([], Map.insert (x, y) id tiles, Map.member (x, y) tiles)
updateTiles val nextTile tiles = (val : nextTile, tiles, False)

jump :: (Int -> Bool) -> State -> State
jump test state
  | test v1 = (memory, v2, r, i, o, h, n, t)
  | otherwise = (memory, address + 3, r, i, o, h, n, t)
  where
    (memory, address, r, i, o, h, n, t) = state
    v1 = getWithParam 0 state
    v2 = getWithParam 1 state

comp :: (Int -> Int -> Bool) -> State -> State
comp op state
  | v1 `op` v2 = (newMem 1, address + 4, r, i, o, h, n, t)
  | otherwise = (newMem 0, address + 4, r, i, o, h, n, t)
  where
    (memory, address, r, i, o, h, n, t) = state
    v1 = getWithParam 0 state
    v2 = getWithParam 1 state
    pos = getPos 2 state
    newMem = insert memory pos

offsetRel :: State -> State
offsetRel state = (memory, address + 2, newRel, i, o, h, n, t)
  where
    (memory, address, rel, i, o, h, n, t) = state
    ps = getParams memory address
    v1 = getWithParam 0 state
    newRel = rel + v1

insert :: Memory -> Int -> Int -> Memory
insert memory pos val = before ++ [val] ++ after
  where
    (before, _ : after) = splitAt pos memory

getWithParam :: Int -> State -> Int
getWithParam i (m, a, r, _, _, _, _, _) = get r (parameterMode i ps) m (a + i + 1)
  where
    ps = getParams m a

getPos :: Int -> State -> Int
getPos offset (m, a, r, _, _, _, _, _)
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
parameterMode position instruction = (instruction `quot` (10 ^ position)) `rem` 10
