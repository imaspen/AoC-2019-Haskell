module Day1.Day1 where

--run :: IO ()
--run = do
--    contents <- readFile "input.txt"
--    let values = getValues contents
--    putStrLn $ show $ part2 14 
--    putStrLn $ show $ part2 1969
--    putStrLn $ show $ part2 100756 
--    putStrLn $ show $ sum $ map part2 values

getValues :: String -> [Integer]
getValues contents = map read (lines contents)

part1 :: [Integer] -> Integer 
part1 masses = sum $ map moduleFuelRequirement masses

part2 :: [Integer] -> Integer
part2 masses = sum $ map fuelRequirement (map moduleFuelRequirement masses)

moduleFuelRequirement :: Integer -> Integer
moduleFuelRequirement mass = (mass `quot` 3) - 2

fuelRequirement :: Integer -> Integer
fuelRequirement mass
    | mass <= 0 = 0
    | otherwise = mass + (fuelRequirement $ moduleFuelRequirement mass)
