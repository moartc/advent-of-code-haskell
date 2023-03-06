import Data.Foldable (Foldable (foldl'), maximumBy)
import Data.List (dropWhileEnd, elemIndex, sort, sortBy)
import Data.Set (Set, delete, elemAt, filter, fromList, insert, member, take, toList, union)
import Debug.Trace (trace)
import GHC.Exception (ratioZeroDenomException)

data Valve = Valve {name :: String, val :: Int, list :: [String]}

data State = State {valveName :: String, time :: [Int], stillToOpen :: Set String} deriving (Eq, Ord)

data State2 = State2 {valveNames :: Set String, time2 :: Int, toOpen :: Set String} deriving (Eq, Ord)

parseInput :: String -> Valve
parseInput inp = Valve valve rate valves
  where
    splitted = words inp
    valve = splitted !! 1
    rate = read $ init $ tail $ dropWhile (/= '=') (splitted !! 4) :: Int
    valves = map (Prelude.take 2) $ drop 9 splitted

restToUnclock :: Set String -> [Valve] -> Int
restToUnclock stillToOpen valves = foldl' (\x y -> x + val y) 0 fil
  where
    fil = Prelude.filter (\v -> member (name v) stillToOpen) valves

type TraverseState = (Set State, Int)

type TraverseState2 = (Set State2, Int)

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

    loop = foldl' (\x1 namee -> trav (head (Prelude.filter (\q -> name q == namee) valves)) (time + 1) points valves listTimePoints stillToOpen limit x1) (updatedMap, part1Answer) sort
      where
        sort = reverse $ sortBy (\x y -> compare (val $ head $ Prelude.filter (\v -> name v == x) valves) (val $ head $ Prelude.filter (\v -> name v == y) valves)) (list valve)

    listTimePoints = ltp ++ [points]

part1 :: String -> Int
part1 inp = snd $ trav start 1 0 parsedInp [] mapped 30 (Data.Set.fromList [], 0)
  where
    parsedInp = map parseInput $ lines inp
    start = head $ Prelude.filter (\q -> name q == "AA") parsedInp
    filterd = Prelude.filter (\q -> val q > 0) parsedInp
    mapped = Data.Set.fromList $ map name filterd

restToUnclock2 :: Set String -> [Valve] -> Int -> [Int] -> Int
restToUnclock2 stillToOpen valves roundToTheEnd opened = sum addRes
  where
    toOpen = reverse (sort $ Prelude.map val $ Prelude.filter (\v -> member (name v) stillToOpen) valves)
    addRes = add toOpen 0 opened roundToTheEnd

add :: [Int] -> Int -> [Int] -> Int -> [Int]
add toOpen round opened roundToTheEnd
  | round == roundToTheEnd = opened
  | length toOpen > 1 = add (tail $ tail toOpen) (round + 1) (opened ++ [last opened + head toOpen + (toOpen !! 1)]) roundToTheEnd
  | not (null toOpen) = add (tail toOpen) (round + 1) (opened ++ [last opened + head toOpen]) roundToTheEnd
  | otherwise = add toOpen (round + 1) (opened ++ [last opened]) roundToTheEnd

trav2 :: Valve -> Valve -> Int -> Int -> [Valve] -> [Int] -> Set String -> Int -> TraverseState2 -> TraverseState2
trav2 valve1 valve2 time points valves ltp stillToOpen limit state@(map, part2Answer)
  | locState `member` map = (updatedMap, part2Answer)
  | part2Answer > rtun = (updatedMap, part2Answer)
  | null stillToOpen = if newSum > part2Answer then (updatedMap, newSum) else (updatedMap, part2Answer)
  | time == limit = if newSum2 > part2Answer then (updatedMap, newSum2) else (updatedMap, part2Answer)
  | otherwise = lastLoop
  where
    rtun = restToUnclock2 stillToOpen valves (limit - time) listTimePoints
    locState = State2 (Data.Set.fromList [name valve1, name valve2]) time stillToOpen
    updatedMap = Data.Set.insert locState map
    newSum = sumListTimePoint + (limit - time) * points
    newSum2 = sumListTimePoint
    sumListTimePoint = sum listTimePoints
    listTimePoints = ltp ++ [points]
    b1 =
      if name valve1 `elem` stillToOpen && name valve2 `elem` stillToOpen
        then trav2 valve1 valve2 (time + 1) (points + val valve1 + val valve2) valves listTimePoints sto limit (updatedMap, part2Answer)
        else (updatedMap, part2Answer)
      where
        sto = Data.Set.delete (name valve2) $ Data.Set.delete (name valve1) stillToOpen
    b2 =
      if name valve1 `elem` stillToOpen
        then foldl' (\x1 namee -> trav2 valve1 (head (Prelude.filter (\q -> name q == namee) valves)) (time + 1) (points + val valve1) valves listTimePoints (Data.Set.delete (name valve1) stillToOpen) limit x1) b1 v2sort
        else b1
    b3 =
      if name valve2 `elem` stillToOpen
        then foldl' (\x1 namee -> trav2 (head (Prelude.filter (\q -> name q == namee) valves)) valve2 (time + 1) (points + val valve2) valves listTimePoints (Data.Set.delete (name valve2) stillToOpen) limit x1) b2 v1sort
        else b2

    lastLoop = foldl' (\x1 namee -> trav2 (head (Prelude.filter (\q -> name q == fst namee) valves)) (head (Prelude.filter (\q -> name q == snd namee) valves)) (time + 1) points valves listTimePoints stillToOpen limit x1) b3 pairs
      where
        pairs = Prelude.filter (uncurry (/=)) [(next1, next2) | next2 <- v2sort, next1 <- v1sort]
    v2sort = reverse $ sortBy ((\x y -> compare (val $ head $ Prelude.filter (\v -> name v == x) valves) (val $ head $ Prelude.filter (\v -> name v == y) valves))) (list valve2)
    v1sort = reverse $ sortBy ((\x y -> compare (val $ head $ Prelude.filter (\v -> name v == x) valves) (val $ head $ Prelude.filter (\v -> name v == y) valves))) (list valve1)
    maxx = maximum [snd b1, snd b2, snd lastLoop, snd b3]

part2 :: String -> Int
part2 inp = snd $ trav2 start start 1 0 parsedInp [] mapped 26 (Data.Set.fromList [], 0)
  where
    parsedInp = map parseInput $ lines inp
    start = head $ Prelude.filter (\q -> name q == "AA") parsedInp
    filterd = Prelude.filter (\q -> val q > 0) parsedInp
    mapped = Data.Set.fromList $ map name filterd

main :: IO ()
main = do
  input <- readFile "inputs/2022/day-16.txt"
  putStrLn $ "Part 1 = " ++ show (part1 input)
  putStrLn $ "Part 2 = " ++ show (part2 input)
