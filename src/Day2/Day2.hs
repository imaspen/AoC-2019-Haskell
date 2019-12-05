module Day2.Day2 where

type Memory = [Int]
type State = (Memory, Int)
type NounVerb = (Int, Int)

part1 :: Memory -> Int
part1 memory = do
    test memory (12, 2)

part2 :: Memory -> Int
part2 memory = do
    let nounVerbs = concat [[(fromIntegral x, fromIntegral y) | y <- [0..99]] | x <- [0..99]]
    let (noun, verb) = head $ dropWhile (\nounVerb -> (test memory nounVerb) /= 19690720) nounVerbs
    100 * noun + verb

test :: Memory -> NounVerb -> Int
test memory (noun, verb) = do
    let (start:_:_:end) = memory
    head $ runProgram ([start, noun, verb] ++ end, 0)

runProgram :: State -> Memory
runProgram (memory, address)
    | (memory !! address) == 1 = run (+) (memory, address)
    | (memory !! address) == 2 = run (*) (memory, address) 
    | (memory !! address) == 99 = memory
    | otherwise = []

run :: (Int -> Int -> Int) -> State -> Memory
run function (memory, address) = do
    let val1 = getValue (memory, address + 1)
    let val2 = getValue (memory, address + 2)
    let insertPos = getAt (memory, address + 3)
    let (before, _ : after) = splitAt insertPos memory
    let newMemory = before ++ [val1 `function` val2] ++ after
    runProgram (newMemory, address + 4)

getAt :: State -> Int
getAt (memory, address) = memory !! address

getValue :: State -> Int
getValue (memory, address) = getAt (memory, getAt (memory, address))
