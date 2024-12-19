module Day23.Day23 where

import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Deque.Strict (Deque)
import qualified Deque.Strict as Deque
import GHC.Exts (fromList)

type Memory = [Int]

type Address = Int

type RelativeBase = Int

type Id = (Int, Bool)

type Packet = (Int, Int, Bool)

type Queue = Deque Packet

data PartialInstruction = AddressOnly Int | AddressAndX Int Int deriving (Eq, Show)

type NetworkState = (Map Int Queue, Map Int PartialInstruction)

type Halted = Bool

type State = (Memory, Address, RelativeBase, Id, NetworkState, Halted)

type PartialState = (Memory, Address, RelativeBase, Id, Halted)

part1 :: Memory -> Int
part1 memory =
  let networkState = setupNetworkState [0 .. 49]
      partialStates = setupPartialStates memory [0 .. 49]
   in stepAll networkState partialStates

setupNetworkState :: [Int] -> NetworkState
setupNetworkState = foldr (\val (queues, r) -> (Map.insert val (fromList []) queues, r)) (Map.empty, Map.empty)

setupPartialStates :: Memory -> [Int] -> [PartialState]
setupPartialStates memory = map (\id -> (memory ++ repeat 0, 0, 0, (id, False), False))

stepAll :: NetworkState -> [PartialState] -> Int
stepAll networkState states =
  let (networkState', states') = List.mapAccumR stepState networkState states
      (queues, _) = networkState'
   in case Map.lookup 255 queues of
        Nothing -> stepAll networkState' states'
        Just queue ->
          case Deque.uncons queue of
            Nothing -> stepAll networkState' states'
            Just ((_, y, _), _) -> y

stepState :: NetworkState -> PartialState -> (NetworkState, PartialState)
stepState networkState (m, a, r, i, h) =
  let (m', a', r', i', networkState', h') = execute (m, a, r, i, networkState, h)
   in (networkState', (m', a', r', i', h'))

step :: State -> State
step state
  | halted = state
  | otherwise = step $ execute state
  where
    (_, _, _, _, _, halted) = state

execute :: State -> State
execute state
  | op == 1 = arithmetic (+) state
  | op == 2 = arithmetic (*) state
  | op == 3 = getInput state
  | op == 4 = putOutput state
  | op == 5 = jump (/= 0) state
  | op == 6 = jump (== 0) state
  | op == 7 = comp (<) state
  | op == 8 = comp (==) state
  | op == 9 = offsetRel state
  | op == 99 = (memory, address, rel, input, output, True)
  where
    (memory, address, rel, input, output, halted) = state
    op = opcode $ getAt memory address

arithmetic :: (Int -> Int -> Int) -> State -> State
arithmetic op state = (newMem, address + 4, r, i, o, h)
  where
    (memory, address, r, i, o, h) = state
    v1 = getWithParam 0 state
    v2 = getWithParam 1 state
    pos = getPos 2 state
    newMem = insert memory pos (v1 `op` v2)

getInput :: State -> State
getInput state =
  let (memory, address, r, id, networkState, h) = state
      (val, id', networkState') = getInputVal id networkState
      pos = getPos 0 state
      newMem = insert memory pos val
   in (newMem, address + 2, r, id', networkState', h)

getInputVal :: Id -> NetworkState -> (Int, Id, NetworkState)
getInputVal (id, False) ns = (id, (id, True), ns)
getInputVal (id, True) (queues, partials) =
  let queue = queues Map.! id
      (val, queue') =
        case Deque.uncons queue of
          Nothing -> (-1, queue)
          Just ((x, y, partRead), queue') ->
            if partRead
              then (y, queue')
              else (x, Deque.cons (x, y, True) queue')
      queues' = Map.insert id queue' queues
   in (val, (id, True), (queues', partials))

putOutput :: State -> State
putOutput state = (memory, address + 2, r, i, networkState', h)
  where
    (memory, address, r, i, (queues, partials), h) = state
    (id, _) = i
    val = getWithParam 0 state
    networkState' =
      case Map.lookup id partials of
        Nothing -> (queues, Map.insert id (AddressOnly val) partials)
        Just (AddressOnly addr) -> (queues, Map.insert id (AddressAndX addr val) partials)
        Just (AddressAndX addr x) -> (Map.alter (updateQueue (x, val)) addr queues, Map.delete id partials)

updateQueue :: (Int, Int) -> Maybe Queue -> Maybe Queue
updateQueue (x, y) Nothing = Just $ fromList [(x, y, False)]
updateQueue (x, y) (Just queue) = Just $ Deque.snoc (x, y, False) queue

jump :: (Int -> Bool) -> State -> State
jump test state
  | test v1 = (memory, v2, r, i, o, h)
  | otherwise = (memory, address + 3, r, i, o, h)
  where
    (memory, address, r, i, o, h) = state
    v1 = getWithParam 0 state
    v2 = getWithParam 1 state

comp :: (Int -> Int -> Bool) -> State -> State
comp op state
  | v1 `op` v2 = (newMem 1, address + 4, r, i, o, h)
  | otherwise = (newMem 0, address + 4, r, i, o, h)
  where
    (memory, address, r, i, o, h) = state
    v1 = getWithParam 0 state
    v2 = getWithParam 1 state
    pos = getPos 2 state
    newMem = insert memory pos

offsetRel :: State -> State
offsetRel state = (memory, address + 2, newRel, i, o, h)
  where
    (memory, address, rel, i, o, h) = state
    ps = getParams memory address
    v1 = getWithParam 0 state
    newRel = rel + v1

insert :: Memory -> Int -> Int -> Memory
insert memory pos val = before ++ [val] ++ after
  where
    (before, _ : after) = splitAt pos memory

getWithParam :: Int -> State -> Int
getWithParam i (m, a, r, _, _, _) = get r (parameterMode i ps) m (a + i + 1)
  where
    ps = getParams m a

getPos :: Int -> State -> Int
getPos offset (m, a, r, _, _, _)
  | mode == 0 = val
  | mode == 2 = val + r
  where
    ps = parameters $ getAt m a
    mode = parameterMode offset ps
    val = getAt m (a + offset + 1)

get :: Int -> Int -> Memory -> Address -> Int
get rel mode
  | mode == 0 = getValue
  | mode == 1 = getAt
  | mode == 2 = getRel rel

getAt :: Memory -> Address -> Int
getAt memory = (memory !!)

getValue :: Memory -> Address -> Int
getValue memory address = getAt memory $ getAt memory address

getRel :: Int -> Memory -> Address -> Int
getRel offset memory address = getAt memory (getAt memory address + offset)

getParams :: Memory -> Address -> Int
getParams m a = parameters $ getAt m a

opcode :: Int -> Int
opcode instruction = instruction `rem` 100

parameters :: Int -> Int
parameters instruction = instruction `quot` 100

parameterMode :: Int -> Int -> Int
parameterMode position instruction = (instruction `quot` (10 ^ position)) `rem` 10
