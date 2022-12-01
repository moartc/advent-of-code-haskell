{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

import Control.Arrow (Arrow (first))
import Control.Concurrent
import Control.Monad
import Data.IntMap hiding (filter, foldl, foldr, map, null, (!))
import Data.List (isInfixOf, sort)
import Data.List.Split (splitOn)
import Data.List.Split.Internals (postProcess)
import Data.Map hiding (drop, filter, foldl, foldr, map, null, take)
import qualified Data.Map as M
import Debug.Trace (trace)
import GHC.IO

data Cache = Cache {floor :: Int, devs :: [String]} deriving (Eq, Show, Ord)

data GameResult = GameResult {foundMinimum :: Int, currentMap :: Map Cache Int} deriving (Eq, Show, Ord)

createState :: Int -> [[String]] -> Cache
createState floorNb floors = Cache floorNb devsString
  where
    devsString = [sort [last dev | dev <- floor] | floor <- floors]

cacheUpdate :: Cache -> Int -> Map Cache Int -> Map Cache Int
cacheUpdate cache counter oldMap = newMap
  where
    newMap = M.insert cache counter oldMap

parseOnePart :: String -> String
parseOnePart s = firstPartAbb : [lastChar]
  where
    splitted = splitOn " " s
    firstPartAbb = head $ last $ init splitted
    lastChar = head $ last splitted

parseWholeLine :: String -> [String]
parseWholeLine line =
  if "nothing relevant" `isInfixOf` line
    then []
    else map parseOnePart splitted
  where
    dropped = drop 27 line
    withoutBeginning = splitOn " and a " dropped
    splitted = concatMap (splitOn " a ") withoutBeginning

parseInput :: String -> [[String]]
parseInput = map parseWholeLine . lines

allM :: [String] -> Bool
allM = foldr (\x -> (&&) (last x == 'm')) True

allG :: [String] -> Bool
allG = foldr (\x -> (&&) (last x == 'g')) True

mixedFloorValid :: [String] -> Bool
mixedFloorValid input = all (== True) [r | f <- input, let r = last f /= 'm' || elem (init f ++ ['g']) input]

isFloorValid :: [String] -> Bool
isFloorValid floor
  | null floor = True
  | allM floor = True
  | allG floor = True
  | otherwise = mixedFloorValid floor

canBringTogether :: String -> String -> Bool
canBringTogether first second
  | last first == 'm' && last second == 'm' = True
  | last first == 'g' && last second == 'g' = True
  | otherwise = init first == init second

remove _ [] = []
remove x (y : ys)
  | x == y = remove x ys
  | otherwise = y : remove x ys

removeAll :: [String] -> [String] -> [String]
removeAll xs arr = foldl (flip remove) arr xs

getValidPairs :: [String] -> [String] -> [String] -> [String]
getValidPairs pairs floorFrom floorTo = if isFloorValid newFloorFrom && isFloorValid newFloorTo then filter (/= "") pairs else []
  where
    filteredDevs = filter (/= "") pairs
    newFloorFrom = removeAll filteredDevs floorFrom
    newFloorTo = floorTo ++ filteredDevs

getDevicesToMove :: [[String]] -> Int -> Int -> [[String]]
getDevicesToMove floors from to = devsToMove
  where
    fromFloor = floors !! from
    toFloor = floors !! to
    pairs = [f : [s] | (f, i) <- zip fromFloor [0 ..], (s, j) <- zip (fromFloor ++ [""]) [0 ..], j > i]
    devsToMove = filter (/= []) [getValidPairs pair fromFloor toFloor | pair <- pairs]

replace :: Int -> a -> [a] -> [a]
replace pos newVal list = take pos list ++ newVal : drop (pos + 1) list

step :: Int -> Int -> [[String]] -> [String] -> [[String]]
step from to floors moves = afterStep
  where
    newFrom = removeAll moves (floors !! from)
    newTo = (floors !! to) ++ moves
    temp1 = replace to newTo floors
    afterStep = replace from newFrom temp1

lastFloorSize :: [[String]] -> Int
lastFloorSize floors = length $ floors !! (length floors - 1)

doSmallPart :: [[String]] -> [[String]] -> Int -> Int -> Int -> Int -> Int -> GameResult -> Bool -> GameResult
doSmallPart (dev : resDevs) allFloors size from to currentMoves minFloor state canChangeMin = doSmallPart resDevs allFloors size from to currentMoves minFloor newGameResultToUse canChangeMin -- `debug` "v1"
  where
    add = if canChangeMin && minFloor == from && null (lists !! from) then 1 else 0
    lists = step from to allFloors dev
    tempRes = play lists size to currentMoves (minFloor + add) state
    minVal = min (foundMinimum tempRes) (foundMinimum state)
    newGameResultToUse = GameResult minVal (currentMap tempRes)
doSmallPart [dev] allFloors size from to currentMoves minFloor state canChangeMin = GameResult minVal (currentMap tempRes)
  where
    add = if canChangeMin && minFloor == from && null (lists !! from) then 1 else 0
    lists = step from to allFloors dev
    tempRes = play lists size to currentMoves (minFloor + add) state
    minVal = min (foundMinimum tempRes) (foundMinimum state)
doSmallPart [] allFloors size from to currentMoves minFloor state canChangeMin = state

play :: [[String]] -> Int -> Int -> Int -> Int -> GameResult -> GameResult
play floors size currentFloor currentMoves minFloor state
  | shouldSkip = startResult
  | currentFloor == length floors -1 && size == lastFloorSize floors = state {foundMinimum = currentMoves}
  | currentFloor == length floors -1 = valWithMinValueDownOne
  | currentFloor == minFloor = doSmallPart list2 floors size currentFloor (currentFloor + 1) (currentMoves + 1) minFloor startResult True
  | otherwise = doSmallPart list2 floors size currentFloor (currentFloor + 1) (currentMoves + 1) minFloor valWithMinValueDownOne False
  where
    shouldSkip = mapContainState && (currentMap state ! currentState <= currentMoves) || currentMoves >= foundMinimum state
    updatedMap = cacheUpdate currentState currentMoves (currentMap state)
    mapContainState = Data.Map.member currentState (currentMap state)
    currentState = createState currentFloor floors
    startResult = GameResult (foundMinimum state) updatedMap
    list1 = getDevicesToMove floors currentFloor (currentFloor - 1)
    valWithMinValueDownOne = doSmallPart list1 floors size currentFloor (currentFloor - 1) (currentMoves + 1) minFloor startResult False
    list2 = getDevicesToMove floors currentFloor (currentFloor + 1)

part1 :: [[String]] -> Int
part1 input = foundMinimum $ play input (sum [length row | row <- input]) 0 0 0 (GameResult 50 M.empty)

part2 :: [[String]] -> [String] -> Int
part2 input add = foundMinimum $ play newInput (sum [length row | row <- newInput]) 0 0 0 (GameResult 100 M.empty)
  where
    newFirst = head input ++ add
    newInput = newFirst : tail input

main :: IO ()
main = do
  input <- readFile "inputs/2016/day-11.txt"
  putStrLn $ "Part 1 = " ++ show (part1 $ parseInput input)
  putStrLn $ "Part 2 = " ++ show (part2 (parseInput input) ["eg", "em", "dg", "dm"])
