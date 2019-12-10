module Day9.Day9 where

type Memory = [Int]
type Address = Int
type Input = [Int]
type Output = [Int]
type Halted = Bool
type State = (Memory, Address, Input, Output, Halted)

part1 :: Memory -> Output
part1 memory input = output
    where (outMem, _, _, output, _) = step (memory, 0, [], [], False)

step :: State -> State
step state
    | halted = state
    | otherwise = step $ execute state
    where (_, _, _, _, halted) = state

execute :: State -> State
execute state
    | op == 1  = arithmetic (+) state
    | op == 2  = arithmetic (*) state
    | op == 3  = getInput state
    | op == 4  = putOutput state
    | op == 5  = jump (/=0) state
    | op == 6  = jump (==0) state
    | op == 7  = comp (<) state
    | op == 8  = comp (==) state
    | op == 99 = (memory, address, input, output, True)
    where
        (memory, address, input, output, halted) = state 
        op = opcode $ getAt memory address

arithmetic :: (Int -> Int -> Int) -> State -> State
arithmetic op (memory, address, i, o, h) = (newMem, address + 4, i, o, h)
    where
        ps = getParams memory address
        v1 = getWithParam 0 ps memory address
        v2 = getWithParam 1 ps memory address
        pos = getAt memory (address + 3)
        newMem = insert memory pos (v1 `op` v2)

getInput :: State -> State
getInput (memory, address, (val : rest), o, h) = (newMem, address + 2, rest, o, h)
    where
        pos = getAt memory (address + 1)
        newMem = insert memory pos val
            
putOutput :: State -> State
putOutput (memory, address, i, output, h) = (memory, address + 2, i, newOut, h)
    where 
        ps = getParams memory address
        val = getWithParam 0 ps memory address
        newOut = val : output

jump :: (Int -> Bool) -> State -> State
jump test (memory, address, i, o, h) 
    | test v1 = (memory, v2, i, o, h)
    | otherwise = (memory, address + 3, i, o, h)
    where
        ps = getParams memory address
        v1 = getWithParam 0 ps memory address
        v2 = getWithParam 1 ps memory address

comp :: (Int -> Int -> Bool) -> State -> State
comp op (memory, address, i, o, h)
    | v1 `op` v2 = (newMem 1, address + 4, i, o, h)
    | otherwise  = (newMem 0, address + 4, i, o, h)
    where
        ps = getParams memory address
        v1 = getWithParam 0 ps memory address
        v2 = getWithParam 1 ps memory address
        pos = getAt memory (address + 3)
        newMem = insert memory pos

insert :: Memory -> Int -> Int -> Memory
insert memory pos val = before ++ [val] ++ after
    where (before, _ : after) = splitAt pos memory

getWithParam :: Int -> Int -> Memory -> Address -> Int
getWithParam i ps m a = get (parameterMode i ps) m (a + i + 1)

get :: Int -> Memory -> Address -> Int
get mode
    | mode == 0 = getValue
    | mode == 1 = getAt

getAt :: Memory -> Address -> Int
getAt memory = (memory !!)

getValue :: Memory -> Address -> Int
getValue memory address = getAt memory $ getAt memory address

getParams :: Memory -> Address -> Int
getParams m a = parameters $ getAt m a

opcode :: Int -> Int
opcode instruction = instruction `rem` 100

parameters :: Int -> Int
parameters instruction = instruction `quot` 100

parameterMode :: Int -> Int -> Int
parameterMode position instruction = (instruction `quot` (10 ^ position)) `rem` 10