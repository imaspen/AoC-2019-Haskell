module Day10.Day10 where

import Data.List.Index
import Data.List (nub)
import Numeric.Limits

-- x, y
data Asteroid = Asteroid Int Int | Empty deriving (Show)

part1 :: [String] -> Int
part1 strings = maximum $ map (getVisibleCount . getViewAngles asteroids) asteroids
    where
        asteroids = toAsteroids strings

toAsteroids :: [String] -> [Asteroid]
toAsteroids strings = concat $ imap toAsteroidRow strings
    where 
        toAsteroidRow y string = filter notEmpty $ imap (toAsteroid y) string
        toAsteroid y x char = if char == '#' then Asteroid x y else Empty
        notEmpty (Empty) = False
        notEmpty (Asteroid _ _) = True

getViewAngles :: [Asteroid] -> Asteroid -> [Float]
getViewAngles asteroids (Asteroid x y) = map getViewAngle asteroids
    where
        getViewAngle (Asteroid theirX theirY)
            | theirX == x && theirY == y = Numeric.Limits.nan
            | otherwise = atan2 (fromIntegral $ theirX - x) (fromIntegral $ theirY - y)

getVisibleCount :: [Float] -> Int
getVisibleCount angles = length . nub $ filter (\x -> not $ isNaN x) angles