module Day14.Day14 where

import qualified Data.Bifunctor
import Data.List (find)
import Data.List.Split (splitOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, isJust)
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace (trace, traceShow, traceShowId)

-- (Amount, Id)
type Value = (Int, String)

-- [Inputs] -> Output
type Recipe = ([Value], Value)

-- Id => Amount
type Spare = Map String Int

-- (Output Amount, [Inputs])
type RecipeEntry = (Int, [Value])

-- Output Id => RecipeEntry
type Recipes = Map String RecipeEntry

-- Id => Amount Needed
type Items = Map String Int

-- Id => Amount Held
type Cache = Map String Int

type SeenCaches = Map Cache Int

part1 :: [String] -> Int
part1 recipeStrings =
  let recipes = foldr addRecipe Map.empty recipeStrings
   in fst (getReagentsForFuel recipes) Map.! "ORE"

part2 :: [String] -> Int
part2 recipeStrings =
  let recipes = foldr addRecipe Map.empty recipeStrings
   in getReagentsForFuelBSearch recipes 0 100000000

getReagentsForFuel :: Recipes -> (Items, Cache)
getReagentsForFuel recipes = getReagentsForItems recipes (Map.singleton "FUEL" 1, Map.empty)

getReagentsForFuelBSearch :: Recipes -> Int -> Int -> Int
getReagentsForFuelBSearch recipes min max
  | abs (min - max) <= 1 = min
  | oreCount < 1000000000000 = getReagentsForFuelBSearch recipes midPoint max
  | otherwise = getReagentsForFuelBSearch recipes min midPoint
  where
    midPoint = (min + max) `div` 2
    out = getReagentsForItems recipes (Map.singleton "FUEL" midPoint, Map.empty)
    oreCount = fst out Map.! "ORE"

getReagentsForItems :: Recipes -> (Items, Cache) -> (Items, Cache)
getReagentsForItems recipes (items, surplus)
  | Map.size items == 1 && Map.member "ORE" items = output
  | otherwise = getReagentsForItems recipes output
  where
    output = Map.foldrWithKey (getReagentsForItemWithCache recipes) (Map.empty, surplus) items

getReagentsForItemWithCache :: Recipes -> String -> Int -> (Items, Cache) -> (Items, Cache)
getReagentsForItemWithCache recipes label amountRequired (items, cache)
  | amountRequiredAfterCache == 0 = (items, nextCache)
  | otherwise =
      let (newReagents, newCache) = getReagentsForItem recipes label amountRequiredAfterCache nextCache
       in (foldr (\(newAmount, newLabel) newItems -> Map.insertWith (+) newLabel newAmount newItems) items newReagents, newCache)
  where
    (amountRequiredAfterCache, nextCache) = case Map.lookup label cache of
      Just amountInCache ->
        let amountToUseFromCache = min amountRequired amountInCache
         in (amountRequired - amountToUseFromCache, Map.adjust (\x -> x - amountToUseFromCache) label cache)
      Nothing -> (amountRequired, cache)

getReagentsForItem :: Recipes -> String -> Int -> Cache -> ([Value], Cache)
getReagentsForItem recipes label amountRequired cache =
  let (outputAmount, inputs) = if label == "ORE" then (1, [(1, "ORE")]) else recipes Map.! label
      (iterations, surplus) = getIterationsAndSurplus amountRequired outputAmount
   in (map (Data.Bifunctor.first (iterations *)) inputs, Map.insertWith (+) label surplus cache)

getIterationsAndSurplus :: Int -> Int -> (Int, Int)
getIterationsAndSurplus amountRequired outputAmount =
  case amountRequired `quotRem` outputAmount of
    (iterations, 0) -> (iterations, 0)
    (iterations, surplus) -> (iterations + 1, outputAmount - surplus)

-- ================================ Input parsing ================================ --

addRecipe :: String -> Recipes -> Recipes
addRecipe string =
  let (inputs, (outputAmount, outputLabel)) = parseRecipe string
   in Map.insert outputLabel (outputAmount, inputs)

parseRecipe :: String -> Recipe
parseRecipe string =
  let (inputs : output : _) = splitOn " => " string
   in (parseInputs inputs, parseValue output)

parseInputs :: String -> [Value]
parseInputs string = map parseValue $ splitOn ", " string

parseValue :: String -> Value
parseValue string =
  let (amount : label : _) = splitOn " " string
   in (read amount, label)
