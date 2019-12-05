module Day3.Day3 where

type Point = (Integer, Integer)
type Instruction = (Char, Integer)
type Edge = (Point, Point)
type Tranformation = (Integer -> Integer -> Integer)

part1 :: [[String]] -> Integer
part1 input = do
    let instructions = map (map getInstruction) input
    let wires = map (getEdges (0,0)) instructions
    let crosses = [(point1,point2) | point1 <- wires !! 0, point2 <- wires !! 1, point1 `overlaps` point2]
    minimum $ map (crossDistance . crossAt) crosses

getInstruction :: String -> Instruction
getInstruction (instruction:distance) = (instruction, read distance)

getEdges :: Point -> [Instruction] -> [Edge]
getEdges point [] = []
getEdges (x, y) ((direction, distance) : instructions)
    | direction == 'R' = calcEdge (x + 1, y) (x + distance, y) 2 instructions
    | direction == 'L' = calcEdge (x - distance, y) (x - 1, y) 1 instructions
    | direction == 'U' = calcEdge (x, y + 1) (x, y + distance) 2 instructions
    | direction == 'D' = calcEdge (x, y - distance) (x, y - 1) 1 instructions
    | otherwise = []

calcEdge :: Point -> Point -> Integer -> [Instruction] -> [Edge]
calcEdge point1 point2 next instructions = (point1, point2) : (getEdges (if next == 1 then point1 else point2) instructions)

overlaps :: Edge -> Edge -> Bool
overlaps edge1 edge2 = (canCross edge1 edge2) && (doCross edge1 edge2)

canCross :: Edge -> Edge -> Bool
canCross edge1 edge2 = ((horizontal edge1) && (vertical edge2)) || ((vertical edge1) && (horizontal edge2))

doCross :: Edge -> Edge -> Bool
doCross ((x11, y11), (x12, y12)) ((x21, y21), (x22, y22)) 
    | horizontal ((x11, y11), (x12, y12)) = y11 `between` (y21, y22) && x21 `between` (x11, x12)
    | vertical ((x11, y11), (x12, y12))   = x11 `between` (x21, x22) && y21 `between` (y11, y12) 
    | otherwise = False

crossAt :: (Edge, Edge) -> Point
crossAt (((x11, y11), (x12, y12)), ((x21, y21), (x22, y22)))
    | horizontal ((x11, y11), (x12, y12)) = (x21, y11)
    | vertical ((x11, y11), (x12, y12)) = (x11, y21)
    | otherwise = (0, 0)

crossDistance :: Point -> Integer
crossDistance (x, y) = (abs x) + (abs y)

between :: Integer -> (Integer, Integer) -> Bool
between a (b, c) = a >= b && a <= c

horizontal :: Edge -> Bool
horizontal ((_, y1), (_, y2)) = y1 == y2

vertical :: Edge -> Bool
vertical ((x1, _), (x2, _)) = x1 == x2
