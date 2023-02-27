import Data.List (dropWhileEnd, elemIndex)
import Data.Set (Set, elemAt, filter, fromList, member, take, toList, insert)
import GHC.Exception (ratioZeroDenomException)
import Debug.Trace ( trace )
import Data.Foldable ( Foldable(foldl') )

debug = flip trace

data Valve = Valve {name :: String, val :: Int, list :: [String]} deriving (Show)

data State = State {valveName :: String, time :: [Int], stillToOpen :: Set String} deriving (Eq, Ord)


parseInput :: String -> Valve
parseInput inp = Valve valve rate valves
  where
    splitted = words inp
    valve = splitted !! 1
    rate = read $ init $ tail $ dropWhile (/= '=') (splitted !! 4) :: Int
    valves =map (Prelude.take 2) $  drop 9 splitted


restToUnclock :: Set String -> [Valve] -> Int
restToUnclock stillToOpen valves = sumToRet
  where
    fil = Prelude.filter (\v -> member (name v) stillToOpen) valves
    sumToRet = sum $ map val fil

type TraverseState = (Set State, Int)

trav :: Valve -> Int -> Int -> [Valve] -> [Int] -> Set String -> Int -> TraverseState -> TraverseState
trav valve time points valves ltp stillToOpen limit state@(map, part1Answer)
  | part1Answer > sum listTimePoints + (limit - time) * (last listTimePoints + restToUnclock stillToOpen valves) - length stillToOpen = state
  | locState `member` map = state
  | null stillToOpen = if newSum > part1Answer then (updatedMap, newSum) else (updatedMap, part1Answer)
  | time == limit = if newSum2 > part1Answer then (updatedMap, newSum2) else (updatedMap, part1Answer)
  | otherwise = if snd tt > snd loop then tt else loop
  where
    locState = State (name valve) listTimePoints stillToOpen
    updatedMap = Data.Set.insert locState map 
    rest = (limit - time) * last listTimePoints
    newSum = newSum2 + rest
    newSum2 = sum listTimePoints
    newStillToOpen = Data.Set.filter (\x -> x /= name valve) stillToOpen
    tt =
      if name valve `elem` stillToOpen && val valve > 0
        then trav valve (time + 1) (points + val valve) valves listTimePoints newStillToOpen limit (updatedMap, part1Answer)
        else (updatedMap, part1Answer)

    loop = foldl' (\x1 namee -> trav (head (Prelude.filter (\q -> name q == namee) valves))  (time + 1) points valves listTimePoints stillToOpen limit x1 ) (updatedMap, part1Answer) (list valve)
      

    listTimePoints = ltp ++ [points]

part1 :: String -> Int
part1 inp = snd $ trav start 1 0 parsedInp [] mapped 30 (Data.Set.fromList [], 0)
  where
    parsedInp = map parseInput $ lines inp
    start = head $ Prelude.filter (\q -> name q == "AA") parsedInp
    filterd = Prelude.filter (\q -> val q > 0) parsedInp
    mapped = Data.Set.fromList $ map name filterd


main :: IO ()
main = do
  input <- readFile "inputs/2022/day-16.txt"
  putStrLn $ "Part 1 = " ++ show (part1 input)

--putStrLn $ "Part 2 = " ++ show (part2 (parseInp input) 4000000)
