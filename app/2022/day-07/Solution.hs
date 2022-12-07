import Data.List (intercalate, isInfixOf)
import Data.List.Split (splitOn)
import Data.Map (Map, elems, empty, insertWith, mapWithKey, (!))

buildMap :: String -> Map String Int
buildMap inp = mapWithKey (\k v -> getTotalForKey k folded) folded
  where
    emptyMap = empty :: Map String [String]
    folded = fst $ foldl (flip parse) (emptyMap, []) $ filter (/= "$ ls") $ lines inp

getTotalForKey :: String -> Map String [String] -> Int
getTotalForKey key m = sum [if "/" `isInfixOf` e then getTotalForKey e m else (read (head $ splitOn " " e) :: Int) | e <- m ! key]

type State = (Map String [String], [String])

removeLast :: State -> State
removeLast (map, list) = (map, init list)

addToState :: State -> String -> State
addToState (map, list) toAdd = (map, list ++ [toAdd])

stateWithMap :: State -> Map String [String] -> State
stateWithMap (_, list) newMap = (newMap, list)

parse :: String -> State -> State
parse line state
  | line == "$ cd .." = removeLast state
  | "$ cd " `isInfixOf` line = addToState state splittedLast
  | otherwise = stateWithMap state newMap
  where
    splittedLast = last $ splitOn " " line
    toAddToList = if "dir" `isInfixOf` line then path ++ "/" ++ splittedLast else line
    newMap = insertWith (++) path [toAddToList] (fst state)
    path = intercalate "/" $ snd state

getPath :: [String] -> String
getPath = intercalate "/"

part1 :: Map String Int -> Int
part1 = sum . filter (<= 100000) . elems

part2 :: Map String Int -> Int
part2 map = minimum $ filter (\x -> map ! "/" - x <= (70000000 - 30000000)) $ elems map

main :: IO ()
main = do
  input <- readFile "inputs/2022/day-07.txt"
  let map = buildMap input
  putStrLn $ "Part 1 = " ++ show (part1 map)
  putStrLn $ "Part 2 = " ++ show (part2 map)
