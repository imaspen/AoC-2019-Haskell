module Day10.Day10 where

import Data.List
import Data.List.Index
import Data.Function (on)
import Numeric.Limits

-- x, y
data Asteroid = Asteroid Int Int | Empty deriving (Show)

instance Eq Asteroid where
    Asteroid x1 y1 == Asteroid x2 y2 = x1 == x2 && y1 == y2

part1 :: [String] -> (Asteroid, Int)
part1 strings = findBase $ toAsteroids strings

part2 :: [String] -> Int
part2 strings = x * 100 + y
    where
        asteroids = toAsteroids strings
        (base, _) = findBase asteroids
        checkAngle (_, angle) = angle < -1/2 * pi
        angles = reverse $ sortBy (compare `on` snd) $ zip asteroids $ getViewAngles asteroids base
        sortedAngles = (takeWhile checkAngle angles) ++ (dropWhile checkAngle angles)
        filteredAngles = filter (\(asteroid, angle) -> asteroid /= base) sortedAngles
        (Asteroid x y) = doShoot filteredAngles (-pi) 0

toAsteroids :: [String] -> [Asteroid]
toAsteroids strings = concat $ imap toAsteroidRow strings
    where 
        toAsteroidRow y string = filter notEmpty $ imap (toAsteroid y) string
        toAsteroid y x char = if char == '#' then Asteroid x y else Empty
        notEmpty (Empty) = False
        notEmpty (Asteroid _ _) = True

getViewAngles :: [Asteroid] -> Asteroid -> [Double]
getViewAngles asteroids (Asteroid x y) = map getViewAngle asteroids
    where
        getViewAngle (Asteroid theirX theirY)
            | theirX == x && theirY == y = Numeric.Limits.nan
            | otherwise = atan2 (fromIntegral $ theirX - x) (fromIntegral $ theirY - y)

getVisibleCount :: [Double] -> Int
getVisibleCount angles = length . nub $ filter (\x -> not $ isNaN x) angles

findBase :: [Asteroid] -> (Asteroid, Int)
findBase asteroids = foldr mostVisible (Empty, -1) asteroids
    where
        mostVisible :: Asteroid -> (Asteroid, Int) -> (Asteroid, Int)
        mostVisible asteroid (maxAsteroid, maxVisible) 
            | visible > maxVisible = (asteroid, visible)
            | otherwise = (maxAsteroid, maxVisible)
                where
                    visible = getVisibleCount $ getViewAngles asteroids asteroid

doShoot :: [(Asteroid, Double)] -> Double -> Int -> Asteroid
doShoot ((asteroid, angle) : remaining) lastAngle count
    | angle == lastAngle = doShoot (remaining ++ [(asteroid, angle)]) lastAngle count
    | count == 199       = asteroid
    | otherwise          = doShoot remaining angle (count + 1)
