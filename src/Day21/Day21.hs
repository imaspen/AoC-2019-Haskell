module Day21.Day21 where

import Data.Char
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace (traceShow, traceShowId)

type Memory = [Int]

type Address = Int

type RelativeBase = Int

type Input = [Int]

type Output = [Int]

type Halted = Bool

type State = (Memory, Address, RelativeBase, Input, Output, Halted)

routine1 :: String
routine1 =
  -- 1: if hole 3 away
  -- 2: and ground 4 away
  -- 3-4: or hole 1 away
  "NOT C J\n\
  \AND D J\n\
  \NOT A T\n\
  \OR T J\n\
  \WALK\n"

routine2 :: String
routine2 =
  -- if B isn't safe
  -- or C isn't safe (we'll need to jump soon)
  -- \^
  -- and D is safe (we'll land somewhere safe if we jump now)
  -- and H is safe (we can jump again in case we land on a 1 tile island)
  -- or A isn't safe (we have no choice but to jump)
  -- \^
  "NOT B J\n\
  \NOT C T\n\
  \OR T J\n\
  \AND D J\n\
  \AND H J\n\
  \NOT A T\n\
  \OR T J\n\
  \RUN\n"

part1 :: Memory -> String
part1 memory =
  let (_, _, _, _, output, _) = step (memory ++ repeat 0, 0, 0, map ord routine1, [], False)
      (head : _) = output
   in if head >= 256 then show head else reverse $ map chr output

part2 :: Memory -> String
part2 memory =
  let (_, _, _, _, output, _) = step (memory ++ repeat 0, 0, 0, map ord routine2, [], False)
      (head : _) = output
   in if head >= 256 then show head else reverse $ map chr output

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
getInput state = (newMem, address + 2, r, rest, o, h)
  where
    (memory, address, r, val : rest, o, h) = state
    pos = getPos 0 state
    newMem = insert memory pos val

putOutput :: State -> State
putOutput state = (memory, address + 2, r, i, newOut, h)
  where
    (memory, address, r, i, output, h) = state
    val = getWithParam 0 state
    newOut = val : output

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
parameterMode position instruction = (instruction `quot` (10 ^ position)) `rem` 10
