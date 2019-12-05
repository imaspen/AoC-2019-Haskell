module Day3.Day3 where

type Point = (Integer, Integer)
type Instruction = (Char, Integer)
type Edge = (Point, Point)
type P2Edge = (Edge, Integer)
type Tranformation = (Integer -> Integer -> Integer)

part1 :: [[String]] -> Integer
part1 input = do
    let instructions = map (map getInstruction) input
    let wires = map (getEdges 0 (0,0)) instructions
    let crosses = [(point1,point2) | point1 <- wires !! 0, point2 <- wires !! 1, point1 `overlaps` point2]
    minimum $ map (crossDistance . crossAt) crosses

part2 :: [[String]] -> Integer
part2 input = do
    let instructions = map (map getInstruction) input
    let wires = map (getEdges 0 (0,0)) instructions
    let crosses = [(point1, point2) | point1 <- wires !! 0, point2 <- wires !! 1, point1 `overlaps` point2]
    minimum $ map ((\(_, d) -> d) . crossAt) crosses

getInstruction :: String -> Instruction
getInstruction (instruction:distance) = (instruction, read distance)

getEdges :: Integer -> Point -> [Instruction] -> [P2Edge]
getEdges len point [] = []
getEdges len (x, y) ((direction, distance) : instructions)
    | direction == 'R' = calcEdge (len + distance) (x + 1, y) (x + distance, y) instructions
    | direction == 'L' = calcEdge (len + distance) (x - 1, y) (x - distance, y) instructions
    | direction == 'U' = calcEdge (len + distance) (x, y + 1) (x, y + distance) instructions
    | direction == 'D' = calcEdge (len + distance) (x, y - 1) (x, y - distance) instructions
    | otherwise = []

calcEdge :: Integer -> Point -> Point -> [Instruction] -> [P2Edge]
calcEdge len point1 point2 instructions = ((point1, point2), len) : (getEdges len point2 instructions)

overlaps :: P2Edge -> P2Edge -> Bool
overlaps (edge1, _) (edge2, _) = (canCross edge1 edge2) && (doCross edge1 edge2)

canCross :: Edge -> Edge -> Bool
canCross edge1 edge2 = ((horizontal edge1) && (vertical edge2)) || ((vertical edge1) && (horizontal edge2))

doCross :: Edge -> Edge -> Bool
doCross ((x11, y11), (x12, y12)) ((x21, y21), (x22, y22)) 
    | horizontal ((x11, y11), (x12, y12)) = y11 `between` (y21, y22) && x21 `between` (x11, x12)
    | vertical ((x11, y11), (x12, y12))   = x11 `between` (x21, x22) && y21 `between` (y11, y12) 
    | otherwise = False

crossAt :: (P2Edge, P2Edge) -> (Point, Integer)
crossAt ((((x11, y11), (x12, y12)), d1), (((x21, y21), (x22, y22)), d2))
    | horizontal ((x11, y11), (x12, y12)) = ((x21, y11), (d1 + (x12 `diff` x21) + d2 + (y22 `diff` y11)))
    | vertical ((x11, y11), (x12, y12))   = ((x11, y21), (d1 + (y12 `diff` y21) + d2 + (x22 `diff` x11)))
    | otherwise = ((0, 0), 0)

diff :: Integer -> Integer -> Integer
diff a b
    | a > b = b - a
    | otherwise = a - b

crossDistance :: (Point, Integer) -> Integer
crossDistance ((x, y), _) = (abs x) + (abs y)

between :: Integer -> (Integer, Integer) -> Bool
between a (b, c)
    | b < c = a >= b && a <= c
    | b > c = a >= c && a <= b
    | otherwise = False

horizontal :: Edge -> Bool
horizontal ((_, y1), (_, y2)) = y1 == y2

vertical :: Edge -> Bool
vertical ((x1, _), (x2, _)) = x1 == x2
