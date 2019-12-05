module Day2.Day2 where

type State = ([Int], Int)

part1 :: [Int] -> Int
part1 memory = do
    let (start:_:_:end) = memory
    head $ runProgram ([start, 12, 2] ++ end, 0)

runProgram :: State -> [Int]
runProgram (memory, address)
    | (memory !! address) == 1 = run (+) (memory, address)
    | (memory !! address) == 2 = run (*) (memory, address) 
    | (memory !! address) == 99 = memory
    | otherwise = []

run :: (Int -> Int -> Int) -> State -> [Int]
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
