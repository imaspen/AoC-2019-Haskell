module Day7.Day7Part2 where

import Data.List (nub)

type Params = Int
type Address = Int
type Signal = Int
type Memory = [Int]
type Inputs = [Int]
type Outputs = [Int]
type State = (Memory, Address)
type MachineIO = (Inputs, Outputs)
type NounVerb = (Int, Int)
type AmpState = (State, Inputs)

part2 :: Memory -> Int
part2 memory = maximum $ map (ampValue . run . combinationToAmpState memory) combinations

ampValue :: (Memory, Outputs) -> Int
ampValue (_, val : _) = val

run :: [AmpState] -> (Memory, Outputs)
run states = runProgram (memory, address) (input ++ [0], []) states
    where (((memory, address), input) : _) = states

runProgram :: State -> MachineIO -> [AmpState] -> (Memory, Outputs)
runProgram state io ampStates
    | operation == 1  = add state params io ampStates
    | operation == 2  = mul state params io ampStates
    | operation == 3  = input state io ampStates
    | operation == 4  = output state params io ampStates
    | operation == 5  = jumpIfTrue state params io ampStates
    | operation == 6  = jumpIfFalse state params io ampStates
    | operation == 7  = lessThan state params io ampStates
    | operation == 8  = equals state params io ampStates
    | operation == 99 = (memory, outputs)
    where (memory, address) = state
          instruction = getAt (memory, address)
          operation = opcode instruction
          params = parameters instruction
          (_, outputs) = io


combinationToAmpState :: Memory -> [Signal] -> [AmpState]
combinationToAmpState memory signals = zip (repeat (memory, 0)) $ map (\x -> [x]) signals

combinations :: [[Int]]
combinations = filter ((5==) . length . nub) $ mapM (const [5..9]) [1..5]

add :: State -> Params -> MachineIO -> [AmpState] -> (Memory, Outputs)
add = basicTask (+)

mul :: State -> Params -> MachineIO -> [AmpState]  -> (Memory, Outputs)
mul = basicTask (*)

basicTask :: (Int -> Int -> Int) -> State -> Params -> MachineIO -> [AmpState] -> (Memory, Outputs)
basicTask function (memory, address) params = 
    runProgram (newMemory, address + 4)
    where val1 = get (parameterMode 0 params) (memory, address + 1)
          val2 = get (parameterMode 1 params) (memory, address + 2)
          insertPos = getAt (memory, address + 3)
          newMemory = insert memory insertPos (val1 `function` val2)

input :: State -> MachineIO -> [AmpState] -> (Memory, Outputs)
input (memory, address) ((add : rest), output) = 
    runProgram (newMemory, address + 2) (rest, output)
    where insertPos = getAt (memory, address + 1)
          newMemory = insert memory insertPos add

output :: State -> Params -> MachineIO -> [AmpState] -> (Memory, Outputs)
output (memory, address) params (input, output) (ourState : nextState : otherStates) =
    runProgram newMachine (newInput ++ [toAdd], newOutput) newStates
    where
        toAdd = get (parameterMode 0 params) (memory, address + 1)
        newOutput = toAdd : output
        newOurState = ((memory, address + 2), input)
        newStates = nextState : otherStates ++ [newOurState]
        (newMachine, newInput) = nextState

jumpIfTrue :: State -> Params -> MachineIO -> [AmpState] -> (Memory, Outputs)
jumpIfTrue = jump (/= 0)

jumpIfFalse :: State -> Params -> MachineIO -> [AmpState] -> (Memory, Outputs)
jumpIfFalse = jump (== 0)

jump :: (Int -> Bool) -> State -> Params -> MachineIO -> [AmpState] -> (Memory, Outputs)
jump test (memory, address) params io
    | test val  = runProgram (memory, jump) io
    | otherwise = runProgram (memory, address + 3) io
    where val   = get (parameterMode 0 params) (memory, address + 1)
          jump  = get (parameterMode 1 params) (memory, address + 2)

lessThan :: State -> Params -> MachineIO -> [AmpState] -> (Memory, Outputs)
lessThan = testVal (<)

equals :: State -> Params -> MachineIO -> [AmpState] -> (Memory, Outputs)
equals = testVal (==)

testVal :: (Int -> Int -> Bool) -> State -> Params -> MachineIO -> [AmpState] -> (Memory, Outputs)
testVal test (memory, address) params
    | val1 `test` val2 = run 1
    | otherwise   = run 0
    where val1 = get (parameterMode 0 params) (memory, address + 1)
          val2 = get (parameterMode 1 params) (memory, address + 2)
          pos  = getAt (memory, address + 3)
          run  = \val -> runProgram (insert memory pos val, address + 4)

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