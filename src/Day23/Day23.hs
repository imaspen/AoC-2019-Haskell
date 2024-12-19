module Day23.Day23 where

import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace (traceShow, traceShowId)
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

-- queues, partial instructions, idle threads, seen nat values, duplicate nat value
data NetworkState = NS1 (Map Int Queue, Map Int PartialInstruction) | NS2 (Map Int Queue, Map Int PartialInstruction, Map Int Int, Set Int, Maybe Int) deriving (Eq, Show)

type Halted = Bool

type State = (Memory, Address, RelativeBase, Id, NetworkState, Halted)

type PartialState = (Memory, Address, RelativeBase, Id, Halted)

part1 :: Memory -> Int
part1 memory =
  let networkState = setupNetworkState [0 .. 49]
      partialStates = setupPartialStates memory [0 .. 49]
   in stepAll networkState partialStates

part2 :: Memory -> Int
part2 memory =
  let networkState = setupNetworkState' [0 .. 49]
      partialStates = setupPartialStates memory [0 .. 49]
   in stepAll networkState partialStates

setupNetworkState :: [Int] -> NetworkState
setupNetworkState = foldr (\val (NS1 (queues, r)) -> NS1 (Map.insert val (fromList []) queues, r)) (NS1 (Map.empty, Map.empty))

setupNetworkState' :: [Int] -> NetworkState
setupNetworkState' = foldr (\val (NS2 (queues, r, i, s, d)) -> NS2 (Map.insert val (fromList []) queues, r, i, s, d)) (NS2 (Map.empty, Map.empty, Map.empty, Set.empty, Nothing))

setupPartialStates :: Memory -> [Int] -> [PartialState]
setupPartialStates memory = map (\id -> (memory ++ repeat 0, 0, 0, (id, False), False))

stepAll :: NetworkState -> [PartialState] -> Int
stepAll (NS1 networkState) states =
  let (networkState', states') = List.mapAccumR stepState (NS1 networkState) states
      NS1 (queues, _) = networkState'
   in case Map.lookup 255 queues of
        Nothing -> stepAll networkState' states'
        Just queue ->
          case Deque.uncons queue of
            Nothing -> stepAll networkState' states'
            Just ((_, y, _), _) -> y
stepAll (NS2 networkState) states =
  let (networkState', states') = List.mapAccumR stepState (NS2 networkState) states
      NS2 (_, _, idle, _, _) = networkState'
      isIdle = all (`isIdIdle` idle) [0 .. 49]
      networkState'' =
        if not isIdle
          then networkState'
          else useNat networkState'
      NS2 (_, _, _, _, duplicate) = networkState''
   in case duplicate of
        Nothing -> stepAll networkState'' states'
        Just y -> y

useNat :: NetworkState -> NetworkState
useNat (NS2 (queues, partials, idle, seen, duplicate)) =
  case Map.lookup 255 queues of
    Nothing -> error "NAT was empty"
    Just queue ->
      case Deque.uncons queue of
        Nothing -> error "NAT was empty"
        Just ((x, y, _), queue) ->
          let queues' = Map.delete 255 $ Map.insert 0 (fromList [(x, y, False)]) queues
              idle' = Map.delete 0 idle
              duplicate' = if Set.member y seen then Just y else duplicate
              seen' = Set.insert y seen
           in NS2 (queues', partials, idle', seen', duplicate')

stepState :: NetworkState -> PartialState -> (NetworkState, PartialState)
stepState networkState (m, a, r, i, h) =
  let (id, _) = i
      isIdle = case networkState of
        NS2 (_, _, idle, _, _) -> isIdIdle id idle
        _ -> False
   in if isIdle
        then (networkState, (m, a, r, i, h))
        else
          let (m', a', r', i', networkState', h') = execute (m, a, r, i, networkState, h)
           in (networkState', (m', a', r', i', h'))

isIdIdle :: Int -> Map Int Int -> Bool
isIdIdle id map =
  case Map.lookup id map of
    Nothing -> False
    Just x -> x > 10

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
getInputVal (id, True) (NS1 (queues, partials)) =
  let queue = queues Map.! id
      (val, queue') =
        case Deque.uncons queue of
          Nothing -> (-1, queue)
          Just ((x, y, partRead), queue') ->
            if partRead
              then (y, queue')
              else (x, Deque.cons (x, y, True) queue')
      queues' = Map.insert id queue' queues
   in (val, (id, True), NS1 (queues', partials))
getInputVal (id, True) (NS2 (queues, partials, idle, seen, duplicate)) =
  let queue = queues Map.! id
      (val, queue', idle') =
        case Deque.uncons queue of
          Nothing -> (-1, queue, Map.alter updateIdle id idle)
          Just ((x, y, partRead), queue') ->
            if partRead
              then (y, queue', idle)
              else (x, Deque.cons (x, y, True) queue', idle)
      queues' = Map.insert id queue' queues
   in (val, (id, True), NS2 (queues', partials, idle', seen, duplicate))

updateIdle :: Maybe Int -> Maybe Int
updateIdle Nothing = Just 1
updateIdle (Just x) = Just (x + 1)

putOutput :: State -> State
putOutput state =
  let (memory, address, r, i, networkState, h) = state
      (id, _) = i
      val = getWithParam 0 state
      networkState' = updateNetworkState networkState id val
   in (memory, address + 2, r, i, networkState', h)

updateNetworkState :: NetworkState -> Int -> Int -> NetworkState
updateNetworkState (NS1 (queues, partials)) id val =
  case Map.lookup id partials of
    Nothing -> NS1 (queues, Map.insert id (AddressOnly val) partials)
    Just (AddressOnly addr) -> NS1 (queues, Map.insert id (AddressAndX addr val) partials)
    Just (AddressAndX addr x) -> NS1 (Map.alter (updateQueue (x, val)) addr queues, Map.delete id partials)
updateNetworkState (NS2 (queues, partials, idle, seen, duplicate)) id val =
  case Map.lookup id partials of
    Nothing -> NS2 (queues, Map.insert id (AddressOnly val) partials, idle, seen, duplicate)
    Just (AddressOnly addr) -> NS2 (queues, Map.insert id (AddressAndX addr val) partials, idle, seen, duplicate)
    Just (AddressAndX addr x) ->
      NS2
        ( if addr == 255 then Map.insert addr (fromList [(x, val, False)]) queues else Map.alter (updateQueue (x, val)) addr queues,
          Map.delete id partials,
          foldr Map.delete idle [id, addr],
          seen,
          duplicate
        )

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
