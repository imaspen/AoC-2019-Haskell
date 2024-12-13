module Day22.Day22 where

import Control.Monad (forM_)
import Control.Monad.ST (ST)
import Data.Array.ST (STUArray)
import qualified Data.Array.ST as STUArray
import Data.Array.Unboxed (UArray)
import qualified Data.Array.Unboxed as UArray
import qualified Data.List as List
import Debug.Trace (traceShowId)
import GHC.Num (integerToInt)

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

part2 :: Integer -> [String] -> Integer
part2 deckSize instructions =
  let (a, b) = polyPow 101741582076661 deckSize $ traceShowId $ getCoefficients deckSize (1, 0) $ map parseInstruction $ reverse instructions
   in (2020 * a + b) `mod` deckSize

getCoefficients :: Integer -> (Integer, Integer) -> [Operation] -> (Integer, Integer)
getCoefficients _ coefficients [] = coefficients
getCoefficients l (a, b) (operation : rest) =
  let (newA, newB) =
        case operation of
          New -> (-a, l - b - 1)
          Cut n -> (a, (b + toInteger n) `mod` l)
          Inc n ->
            let z = modInv (toInteger n) l
             in ((a * z) `mod` l, (b * z) `mod` l)
   in getCoefficients l (newA, newB) rest

polyPow :: Integer -> Integer -> (Integer, Integer) -> (Integer, Integer)
polyPow 0 _ _ = (1, 0)
polyPow shuffleCount deckSize (a, b) =
  if even shuffleCount
    then polyPow (shuffleCount `quot` 2) deckSize (a * a `mod` deckSize, (a * b + b) `mod` deckSize)
    else
      let (c, d) = polyPow (shuffleCount - 1) deckSize (a, b)
       in (a * c `mod` deckSize, (a * d + b) `mod` deckSize)

-- https://github.com/AllAlgorithms/haskell/blob/master/algorithms/math/modInv.hs
modInv :: Integer -> Integer -> Integer
modInv a n = let (_, b, _) = exgcd a n in b `mod` n

-- | Extended Euclidean algorithm.
exgcd :: Integer -> Integer -> (Integer, Integer, Integer)
exgcd a 0 = (a, 1, 0)
exgcd a b = (g, y, x - (a `div` b) * y)
  where
    (g, x, y) = exgcd b (a `mod` b)
