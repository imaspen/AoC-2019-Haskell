module Day10.Day10 where

import Prelude hiding (Rational)
import Data.Ratio hiding (Rational)
import qualified Data.Vector as Vector
import Data.Vector (Vector, (!))

-- x, y
data Point = Point Int Int deriving (Show)
-- m, c
data LineDescriptor = NonVerticle Rational Rational | Verticle Int deriving (Show)
data Space = Empty | Asteroid deriving (Eq)

type StarMap = Vector (Vector Space)
type Rational = Ratio Int

instance Show Space where
    show Empty = "."
    show Asteroid = "#"

part1 :: [String] -> Int
part1 strings = maximumVisible starMap
    where 
        starMap = generateStarMap strings

generateStarMap :: [String] -> StarMap
generateStarMap strings = Vector.fromList $ map Vector.fromList $ map (map charToSpace) strings

charToSpace :: Char -> Space
charToSpace '.' = Empty
charToSpace '#' = Asteroid

maximumVisible :: StarMap -> Int
maximumVisible starMap = Vector.maximum $ Vector.map Vector.maximum $ visibleMap starMap

visibleMap :: StarMap -> Vector (Vector Int)
visibleMap starMap = Vector.imap (\y row -> Vector.imap (\x space -> getVisibleFromPoint starMap (Point x y) space) row) starMap

getVisibleFromPoint :: StarMap -> Point -> Space -> Int
getVisibleFromPoint _ _ Empty = 0
getVisibleFromPoint starMap p Asteroid = (Vector.ifoldl (foldRow starMap p) 0 starMap) - 1

foldRow :: StarMap -> Point -> Int -> Int -> Vector Space -> Int
foldRow starMap p acc y row = Vector.ifoldl (countVisible starMap p y) acc row

countVisible :: StarMap -> Point -> Int -> Int -> Int -> Space -> Int
countVisible starMap p y acc x _
    | isPointVisible starMap p $ Point x y = acc + 1
    | otherwise = acc

isPointVisible :: StarMap -> Point -> Point -> Bool
isPointVisible starMap p1 p2 
    | checkIntersects starMap p2 = checkIntersections intersections starMap
    | otherwise = False
    where
        line = constructLine p1 p2
        intersections = getNaturalIntersections line p1 p2

getNaturalIntersections :: LineDescriptor -> Point -> Point -> [Point]
getNaturalIntersections (Verticle x) (Point _ y1) (Point _ y2) =
    [Point x y | y <- (getRange y1 y2)]
getNaturalIntersections l (Point x1 _) (Point x2 _) = 
    [Point x (numerator $ getY l x) | x <- (getRange x1 x2), (denominator $ getY l x) == 1]

getRange :: Int -> Int -> [Int]
getRange x y
    | x <= y    = [x+1..y-1]
    | otherwise = [y+1..x-1]

checkIntersections :: [Point] -> StarMap -> Bool
checkIntersections points starMap = null intersections
    where intersections = filter (checkIntersects starMap) points

checkIntersects :: StarMap -> Point -> Bool
checkIntersects starMap (Point x y) = ((starMap ! y) ! x) == Asteroid

constructLine :: Point -> Point -> LineDescriptor
constructLine p1 p2
    | x1 == x2  = Verticle x1
    | otherwise = constructNonVerticalLine p1 p2
    where
        (Point x1 _) = p1
        (Point x2 _) = p2

constructNonVerticalLine :: Point -> Point -> LineDescriptor
constructNonVerticalLine p1 p2 = NonVerticle m c
    where
        m = getM p1 p2
        c = getC p1 m

getM :: Point -> Point -> Rational
getM (Point x1 y1) (Point x2 y2) = (y2 - y1) % (x2 - x1)

getC :: Point -> Rational -> Rational
getC (Point x y) m = (y % 1) - (m * (x % 1))

getY :: LineDescriptor -> Int -> Rational
getY (NonVerticle m c) x = (m * (x % 1)) + c