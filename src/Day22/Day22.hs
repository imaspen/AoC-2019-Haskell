module Day22.Day22 where

import Control.Monad (forM_)
import Control.Monad.ST (ST)
import Data.Array.ST (STUArray)
import qualified Data.Array.ST as STUArray
import Data.Array.Unboxed (UArray)
import qualified Data.Array.Unboxed as UArray
import qualified Data.List as List

data Operation = New | Cut Int | Inc Int deriving (Show)

type Stack = UArray Int Int

type MStack s = STUArray s Int Int

part1 :: Int -> [String] -> Int
part1 deckSize instructions =
  let Just x = List.elemIndex 2019 $ part1List deckSize instructions
   in x

part1List :: Int -> [String] -> [Int]
part1List deckSize instructions = UArray.elems $ run deckSize $ map parseInstruction instructions

run :: Int -> [Operation] -> UArray Int Int
run deckSize instructions = STUArray.runSTUArray $ do
  curr <- startingArray deckSize
  new <- startingArray deckSize
  step instructions curr new

parseInstruction :: String -> Operation
parseInstruction input =
  case List.stripPrefix "cut " input of
    Just x -> Cut $ read x
    Nothing -> case List.stripPrefix "deal with increment " input of
      Just x -> Inc $ read x
      Nothing -> New

startingArray :: Int -> ST s (MStack s)
startingArray size = STUArray.newListArray (0, size - 1) [0 ..]

step :: [Operation] -> MStack s -> MStack s -> ST s (MStack s)
step [] curr new = do return curr
step (operation : rest) curr new = do
  applyOperation operation curr new
  step rest new curr

applyOperation :: Operation -> MStack s -> MStack s -> ST s ()
applyOperation operation stack newStack =
  do
    (start, end) <- STUArray.getBounds stack
    forM_ [start .. end] $ \i -> do
      curr <- STUArray.readArray stack i
      case operation of
        New -> STUArray.writeArray newStack (end - i) curr
        Cut x -> STUArray.writeArray newStack ((i - x) `mod` (end + 1)) curr
        Inc x -> STUArray.writeArray newStack ((i * x) `mod` (end + 1)) curr
