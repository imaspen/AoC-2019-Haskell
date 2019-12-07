module Day5.Day5 where

type Params = Int
type Memory = [Int]
type Inputs = [Int]
type Outputs = [Int]
type State = (Memory, Int)
type MachineIO = (Inputs, Outputs)
type NounVerb = (Int, Int)

part1 :: Memory -> Inputs -> Outputs
part1 memory inputs = outputs
    where (_, outputs) = runProgram (memory, 0) (inputs, [])

runProgram :: State -> MachineIO -> (Memory, Outputs)
runProgram state io
    | operation == 1  = add state params io
    | operation == 2  = mul state params io
    | operation == 3  = input state io
    | operation == 4  = output state params io
    | operation == 5  = jumpIfTrue state params io
    | operation == 6  = jumpIfFalse state params io
    | operation == 7  = lessThan state params io
    | operation == 8  = equals state params io
    | operation == 99 = (memory, outputs)
    | otherwise = ([], [])
    where (memory, address) = state
          instruction = getAt (memory, address)
          operation = opcode instruction
          params = parameters instruction
          (_, outputs) = io

add :: State -> Params -> MachineIO -> (Memory, Outputs)
add = basicTask (+)

mul :: State -> Params -> MachineIO -> (Memory, Outputs)
mul = basicTask (*)

basicTask :: (Int -> Int -> Int) -> State -> Params -> MachineIO -> (Memory, Outputs)
basicTask function (memory, address) params io = 
    runProgram (newMemory, address + 4) io
    where val1 = get (parameterMode 0 params) (memory, address + 1)
          val2 = get (parameterMode 1 params) (memory, address + 2)
          insertPos = getAt (memory, address + 3)
          newMemory = insert memory insertPos (val1 `function` val2)

input :: State -> MachineIO -> (Memory, Outputs)
input (memory, address) ((add : rest), output) = 
    runProgram (newMemory, address + 2) (rest, output)
    where insertPos = getAt (memory, address + 1)
          newMemory = insert memory insertPos add

output :: State -> Params -> MachineIO -> (Memory, Outputs)
output (memory, address) params (input, output) = 
    runProgram (memory, address + 2) (input, newOutput)
    where toAdd = get (parameterMode 0 params) (memory, address + 1)
          newOutput = toAdd : output

jumpIfTrue :: State -> Params -> MachineIO -> (Memory, Outputs)
jumpIfTrue = jump (/= 0)

jumpIfFalse :: State -> Params -> MachineIO -> (Memory, Outputs)
jumpIfFalse = jump (== 0)

jump :: (Int -> Bool) -> State -> Params -> MachineIO -> (Memory, Outputs)
jump test (memory, address) params io
    | test val  = runProgram (memory, jump) io
    | otherwise = runProgram (memory, address + 3) io
    where val   = get (parameterMode 0 params) (memory, address + 1)
          jump  = get (parameterMode 1 params) (memory, address + 2)

lessThan :: State -> Params -> MachineIO -> (Memory, Outputs)
lessThan = testVal (<)

equals :: State -> Params -> MachineIO -> (Memory, Outputs)
equals = testVal (==)

testVal :: (Int -> Int -> Bool) -> State -> Params -> MachineIO -> (Memory, Outputs)
testVal test (memory, address) params io
    | val1 `test` val2 = run 1
    | otherwise   = run 0
    where val1 = get (parameterMode 0 params) (memory, address + 1)
          val2 = get (parameterMode 1 params) (memory, address + 2)
          pos  = getAt (memory, address + 3)
          run  = \val -> runProgram (insert memory pos val, address + 4) io

insert :: Memory -> Int -> Int -> Memory
insert memory pos val = before ++ [val] ++ after
    where (before, _ : after) = splitAt pos memory

get :: Int -> State -> Int
get mode state
    | mode == 0 = getValue state
    | mode == 1 = getAt state

getAt :: State -> Int
getAt (memory, address) = memory !! address

getValue :: State -> Int
getValue (memory, address) = getAt (memory, getAt (memory, address))

opcode :: Int -> Int
opcode instruction = instruction `rem` 100

parameters :: Int -> Int
parameters instruction = instruction `quot` 100

parameterMode :: Int -> Int -> Int
parameterMode position instruction = (instruction `quot` (10 ^ position)) `rem` 10