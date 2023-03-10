import Data.List.Split (splitOn)
import Data.Set (Set, delete, empty, fromList, insert, null, take, toList)

data Cube = Cube {x :: Int, y :: Int, z :: Int} deriving (Show, Eq, Ord)

parseInp :: String -> [Cube]
parseInp inp = map (\i -> Cube (head i) (i !! 1) (i !! 2)) ints
  where
    ints = map (map (\x -> read x :: Int) . splitOn ",") $ lines inp

part1 :: String -> Int
part1 input = sum $ map (\c -> totalExposed c cubes []) cubes
  where
    cubes = parseInp input

totalExposed :: Cube -> [Cube] -> [Cube] -> Int
totalExposed c1 l t = foldl (\ctr q -> if q `elem` l || q `elem` t then ctr - 1 else ctr) 6 all
  where
    x1 = Cube (x c1 -1) (y c1) (z c1)
    x2 = Cube (x c1 + 1) (y c1) (z c1)
    y1 = Cube (x c1) (y c1 + 1) (z c1)
    y2 = Cube (x c1) (y c1 -1) (z c1)
    z1 = Cube (x c1) (y c1) (z c1 + 1)
    z2 = Cube (x c1) (y c1) (z c1 -1)
    all = [x1, x2, y1, y2, z1, z2]

-------------------------------- PART 2 --------------------------------

getNeighbour :: Cube -> [Cube]
getNeighbour cube = [c1, c2, c3, c4, c5, c6]
  where
    c1 = Cube (x cube -1) (y cube) (z cube)
    c2 = Cube (x cube + 1) (y cube) (z cube)
    c3 = Cube (x cube) (y cube + 1) (z cube)
    c4 = Cube (x cube) (y cube -1) (z cube)
    c5 = Cube (x cube) (y cube) (z cube + 1)
    c6 = Cube (x cube) (y cube) (z cube -1)

part2 :: [Char] -> Int
part2 input = sum $ map (\c -> totalExposed c cubes filtered) cubes
  where
    cubes = parseInp input
    xRange = (minimum (map x cubes) - 1, maximum (map x cubes))
    yRange = (minimum (map y cubes) - 1, maximum (map y cubes))
    zRange = (minimum (map z cubes) - 1, maximum (map z cubes))
    queue = [Cube (fst xRange) (fst yRange) (fst zRange)]
    (resCube, resCollection) = lp cubes xRange yRange zRange (Data.Set.fromList queue, Data.Set.empty)
    filtered = filter (\q -> q `notElem` cubes && q `notElem` resCollection) $ [Cube x y z | x <- [fst xRange .. snd xRange], y <- [fst yRange .. snd yRange], z <- [fst zRange .. snd zRange]]

lp :: [Cube] -> (Int, Int) -> (Int, Int) -> (Int, Int) -> (Set Cube, Set Cube) -> (Set Cube, Set Cube)
lp cubes xR yR zR (queue, airCollection)
  | Data.Set.null queue = (queue, airCollection)
  | otherwise = lp cubes xR yR zR (newQueue2, Data.Set.insert current airCollection)
  where
    current = head $ Data.Set.toList queue
    filt = filter (\q -> q `notElem` airCollection && inRange q xR yR zR && q `notElem` cubes) $ getNeighbour current
    newQueue2 = Data.Set.fromList $ Data.Set.toList (Data.Set.delete current queue) ++ filt

inRange :: Cube -> (Int, Int) -> (Int, Int) -> (Int, Int) -> Bool
inRange cube xRange yRange zRange = xOk && yOk && zOk
  where
    xOk = x cube >= fst xRange && x cube <= snd xRange
    yOk = y cube >= fst yRange && y cube <= snd yRange
    zOk = z cube >= fst zRange && z cube <= snd zRange

main :: IO ()
main = do
  input <- readFile "inputs/2022/day-18.txt"
  putStrLn $ "Part 1 = " ++ show (part1 input)
  putStrLn $ "Part 2 = " ++ show (part2 input)
