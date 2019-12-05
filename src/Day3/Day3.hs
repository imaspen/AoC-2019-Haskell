module Day3.Day3 where

type Point = (Integer, Integer)
type Instruction = (Char, Integer)

part1 :: [[String]] -> Integer
part1 input = do
    let instructions = map (map getInstruction) input
    let wires = map (run [(0,0)]) instructions
    minimum $ map (\(x, y) -> (abs x) + (abs y)) [point1 | point1 <- wires !! 0, point2 <- wires !! 1, point1 == point2 && point1 /= (0, 0)]

getInstruction :: String -> Instruction
getInstruction (instruction:distance) = (instruction, read distance)

run :: [Point] -> [Instruction] -> [Point]
run point [] = point 
run point (instruction:instructions) = do
    let path = doInstruction point instruction
    run path instructions

doInstruction :: [Point] -> Instruction -> [Point]
doInstruction path (direction, distance)
    | distance == 0 = path
    | direction == 'R' = move path (direction, distance) (+) (*)
    | direction == 'L' = move path (direction, distance) (-) (*)
    | direction == 'U' = move path (direction, distance) (*) (+)
    | direction == 'D' = move path (direction, distance) (*) (-)
    | otherwise = []

move :: [Point] -> Instruction -> (Integer -> Integer -> Integer) -> (Integer -> Integer -> Integer) -> [Point]
move path (direction, distance) xOp yOp = do
    let (x, y) = head path
    doInstruction ((x `xOp` 1, y `yOp` 1):path) (direction, distance - 1)
