{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

import Control.Arrow (Arrow (first))
import Data.List (isInfixOf)
import Data.List.Split (splitOn)

-- input :: [Char]
-- input = "The first floor contains a strontium generator, a strontium-compatible microchip, a plutonium generator, and a plutonium-compatible microchip.\nThe second floor contains a thulium generator, a ruthenium generator, a ruthenium-compatible microchip, a curium generator, and a curium-compatible microchip.\nThe third floor contains a thulium-compatible microchip.\nThe fourth floor contains nothing relevant."

type Floor = [String]

data State = State {floors :: [Floor], currentFloor :: Int, numsofMoves :: Int, lowestFloor :: Int} deriving (Show)

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
parseInput input = map parseWholeLine $ lines input

allM :: [String] -> Bool
allM = foldr (\x -> (&&) (last x == 'm')) True

allG :: [String] -> Bool
allG = foldr (\x -> (&&) (last x == 'g')) True

mixedFloorValid :: [String] -> Bool
mixedFloorValid input = all (== True) [r | f <- input, let r = last f /= 'm' || elem (init f ++ ['g']) input]

isFloorValid :: Floor -> Bool
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

getValidPairs :: [String] -> Floor -> Floor -> [String]
getValidPairs pairs floorFrom floorTo = if isFloorValid newFloorFrom && isFloorValid newFloorTo then filter (/= "") pairs else []
  where
    filteredDevs = filter (/= "") pairs
    newFloorFrom = removeAll filteredDevs floorFrom
    newFloorTo = floorTo ++ filteredDevs

getDevicesToMove :: [Floor] -> Int -> Int -> [[String]]
getDevicesToMove floors from to = filter (/= []) [getValidPairs pair fromFloor toFloor | pair <- pairs]
  where
    fromFloor = floors !! from
    toFloor = floors !! to
    pairs = [f : [s] | (f, i) <- zip fromFloor [0 ..], (s, j) <- zip (fromFloor ++ [""]) [0 ..], j > i]

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



main :: IO ()
main = do
  input <- readFile "inputs/day-11.txt"
  putStrLn $ "Part 1 = " ++ show (parseInput input)
