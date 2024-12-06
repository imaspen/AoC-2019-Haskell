module Day18.Day18 where

import qualified Data.Char as Char
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace (traceShow)
import Deque.Strict (Deque)
import qualified Deque.Strict as Deque
import GHC.Exts (fromList)

type Point = (Int, Int)

type Key = Char

type Door = Char

type AllKeys = Set Key

type HeldKeys = Set Key

data Location = Empty | Key Key | Door Door deriving (Eq, Show)

type Vault = Map Point Location

data State = State
  { position :: Point,
    collectedKeys :: Set Key,
    distance :: Int
  }
  deriving (Show)

type Discovery = (Point, HeldKeys)

type Discovered = Set Discovery

type Queue = Deque State

part1 :: [String] -> Int
part1 lines =
  let nonEmpty = filter (/= "") lines
      (vault, keys, startPoint) = parseInput nonEmpty
      queue = fromList [State startPoint Set.empty 0]
      out = search vault keys queue Set.empty
   in out

search :: Vault -> AllKeys -> Queue -> Discovered -> Int
search vault allKeys queue discovered =
  if Deque.null queue
    then 0
    else
      let Just (state, rest) = Deque.uncons queue
          adj = getAdjacencies vault state
          notDiscovered = filter (\x -> not $ isDiscovered x discovered) adj
          newDiscovered = foldr (Set.insert . stateToDiscovery) discovered notDiscovered
          newQueue = foldr Deque.snoc rest notDiscovered
       in if collectedKeys state == allKeys
            then distance state
            else search vault allKeys newQueue newDiscovered

stateToDiscovery :: State -> Discovery
stateToDiscovery state = (position state, collectedKeys state)

isDiscovered :: State -> Discovered -> Bool
isDiscovered state = Set.member (stateToDiscovery state)

getAdjacencies :: Vault -> State -> [State]
getAdjacencies vault state =
  let (x, y) = position state
      heldKeys = collectedKeys state
      dist = 1 + distance state
   in Maybe.mapMaybe (isAdjacent vault heldKeys dist) [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

isAdjacent :: Vault -> HeldKeys -> Int -> Point -> Maybe State
isAdjacent vault heldKeys dist point = case Map.lookup point vault of
  Nothing -> Nothing
  Just Empty -> Just $ State point heldKeys dist
  Just (Key k) -> Just $ State point (Set.insert k heldKeys) dist
  Just (Door d) -> if Set.member d heldKeys then Just $ State point heldKeys dist else Nothing

-------- PARSING --------

parseInput :: [String] -> (Vault, AllKeys, Point)
parseInput lines = foldr (uncurry parseLine) (Map.empty, Set.empty, (0, 0)) $ zip [0 ..] lines

parseLine :: Int -> String -> (Vault, AllKeys, Point) -> (Vault, AllKeys, Point)
parseLine y line vault = foldr (uncurry $ parseChar y) vault $ zip [0 ..] line

parseChar :: Int -> Int -> Char -> (Vault, AllKeys, Point) -> (Vault, AllKeys, Point)
parseChar y x char (vault, keys, startPos)
  | char == '#' = (vault, keys, startPos)
  | char == '.' = (Map.insert (x, y) Empty vault, keys, startPos)
  | char == '@' = (Map.insert (x, y) Empty vault, keys, (x, y))
  | Char.isLower char = (Map.insert (x, y) (Key $ Char.toUpper char) vault, Set.insert (Char.toUpper char) keys, startPos)
  | Char.isUpper char = (Map.insert (x, y) (Door char) vault, keys, startPos)
