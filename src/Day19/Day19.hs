module Day19.Day19 where

type Memory = [Int]

type Address = Int

type RelativeBase = Int

type Input = [Int]

type Output = [Int]

type Halted = Bool

type State = (Memory, Address, RelativeBase, Input, Output, Halted)

part1 :: Memory -> Int
part1 memory = sum $ map (\y -> sum $ map (\x -> head $ runIntCode memory [x, y]) [0 .. 49]) [0 .. 49]

-- derived formulas from inspecting outputs and poking coordinates
-- right line -> x = 2y
-- left line -> x = (17/11)y

-- formula for top right x
-- x = 2y
-- bottom left in relation to top right
-- x - 99 = (17/11) * (y + 99)
-- 11x - 1089 = 17 * (y + 99)
-- 11x - 1089 = 17y + 1683
-- 11x = 17y + 2772
-- x = (17y + 2772) / 11
-- 2y = (17y + 2772) / 11
-- 22y = 17y + 2772
-- 5y = 2772
-- y = 554.4
-- round up, top-right y = 555
-- top-right x = 1110
-- top-left x = 1011

part2 :: Int
part2 = 1011 * 10000 + 555

printMap :: Memory -> String
printMap memory =
  let output = map (\y -> map (\x -> head $ runIntCode memory [x, y]) [0 .. 49]) [0 .. 49]
      o = map (\line -> '\n' : map (\x -> if x == 1 then '#' else '.') line) output
   in concat o

runIntCode :: Memory -> Input -> Output
runIntCode memory input =
  let (_, _, _, _, output, _) = step (memory ++ repeat 0, 0, 0, input, [], False)
   in output

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
parameterMode position instruction = instruction `quot` 10 ^ position `rem` 10
