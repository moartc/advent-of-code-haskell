import Data.Char
import Data.Either (fromRight)
import Data.List (sort)

type Grid = [[Int]]

type Queue = [(Int, Int)]

type PathCost = [((Int, Int), Int)]

add :: [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
add queue el = el : queue

remove :: [(Int, Int)] -> ((Int, Int), [(Int, Int)])
remove queue = (el, rest)
  where
    el = last queue
    rest = init queue

shortestPath :: Grid -> (Int, Int) -> (Int, Int) -> Int
shortestPath grid (sY, sX) (eY, eX) = case processQueue (Right (updatedGrid, [(sY, sX)])) (eY, eX) of
  Left n -> n
  Right b -> (-1)
  where
    updatedGrid = updateGrid grid sY sX 0
    moves = [(-1, 0), (1, 0), (0, 1), (0, -1)]

processQueue :: Either Int (Grid, Queue) -> (Int, Int) -> Either Int (Grid, Queue)
processQueue either (yToFind, xToFind) = case either of
  Left n -> Left n
  Right (newGrid, newQueue) -> if null newQueue then Left (-1) else let (rem, newestQueue) = remove newQueue in processQueue (allMoves moves rem newestQueue newGrid yToFind xToFind) (yToFind, xToFind)
  where
    moves = [(-1, 0), (1, 0), (0, 1), (0, -1)]

allMoves :: [(Int, Int)] -> (Int, Int) -> [(Int, Int)] -> [[Int]] -> Int -> Int -> Either Int (Grid, Queue)
allMoves (m : ms) removed queue grid yToFind xToFind = case single of
  Left n -> single
  Right (newGrid, newQueue) -> allMoves ms removed newQueue newGrid yToFind xToFind
  where
    single = singleMove m removed queue grid yToFind xToFind
allMoves [] _ queue grid _ _ = Right (grid, queue)

singleMove :: (Int, Int) -> (Int, Int) -> [(Int, Int)] -> [[Int]] -> Int -> Int -> Either Int (Grid, Queue)
singleMove move removed queue grid yToFind xToFind
  | (nextY < 0 || nextY >= lngY || nextX < 0 || nextX >= lngX) || (grid !! nextY !! nextX) /= 0 = Right (grid, queue)
  | nextY == yToFind && nextX == xToFind = Left $ (grid !! fst removed !! snd removed) + 1
  | otherwise = Right (updatedGrid, updatedQueue)
  where
    lngY = length grid
    lngX = length (head grid)
    nextY = fst removed + fst move
    nextX = snd removed + snd move
    valToAdd = (grid !! fst removed !! snd removed) + 1
    updatedGrid = updateGrid grid nextY nextX valToAdd
    updatedQueue = add queue (nextY, nextX)

findCoord :: [String] -> Int -> (Int, Int)
findCoord grid val = (y, x)
  where
    charToFind = intToDigit val
    (line, y) = head [(l, i) | (l, i) <- zip grid [0 ..], charToFind `elem` l]
    (c, x) = head [(c, x) | (c, x) <- zip line [0 ..], c == charToFind]

convertLine :: String -> [Int]
convertLine line = [if c == '#' then 1 else 0 | c <- line]

updateGrid :: Grid -> Int -> Int -> Int -> Grid
updateGrid grid iY iX val = [if i == iY then updateRow row iX val else row | (row, i) <- zip grid [0 ..]]

updateRow :: [Int] -> Int -> Int -> [Int]
updateRow row iX val = [if i == iX then val else v | (v, i) <- zip row [0 ..]]

generateCosts :: [String] -> Int -> Int -> PathCost
generateCosts input fIdx lIdx = [((f, s), shortestPath grid (sY, sX) (eY, eX)) | (f, s) <- allPairs, let (sY, sX) = findCoord input f, let (eY, eX) = findCoord input s]
  where
    allPairs = [(s, f) | f <- [fIdx .. lIdx], s <- [fIdx .. lIdx], f /= s]
    grid = map convertLine input

createPath :: [PathCost] -> PathCost -> Int -> [PathCost]
createPath inp costs l
  | length (head inp) == l = inp
  | otherwise = createPath (addNext inp costs) costs l

addNext :: [PathCost] -> PathCost -> [PathCost]
addNext generated costs = filter (not . null) [if canAdd s x then s ++ [x] else [] | s <- generated, x <- costs]

canAdd :: PathCost -> ((Int, Int), Int) -> Bool
canAdd list ((a, b), _) = (null list && a == 0) || (not (null list) && y == a && not alreadyVisited)
  where
    ((_, y), _) = last list
    alreadyVisited = b `elem` ([x | ((x, _), _) <- list])

createPath2 :: [PathCost] -> PathCost -> Int -> [PathCost]
createPath2 inp costs l
  | length (head inp) == l = inp
  | otherwise = createPath2 (addNext2 inp costs (length (head inp) == (l -1))) costs l  

addNext2 :: [PathCost] -> PathCost -> Bool -> [PathCost]
addNext2 generated costs isLast = filter (not . null) [if canAdd2 s x isLast then s ++ [x] else [] | s <- generated, x <- costs]  

canAdd2 :: PathCost -> ((Int, Int), Int) -> Bool -> Bool
canAdd2 list ((a, b), _) isLast = (null list && a == 0) || (not (null list) && ((y == a && not alreadyVisited) || (y == a && b == 0 && alreadyVisited && isLast)))
  where
    ((_, y), _) = last list
    alreadyVisited = b `elem` ([x | ((x, _), _) <- list])

getMin :: [PathCost] -> Int
getMin = minimum . map sumArr

sumArr :: PathCost -> Int
sumArr inp = sum [x | ((a, b), x) <- inp]

getAllNodes :: [Char] -> [Int]
getAllNodes = sort . map digitToInt . filter isDigit

part1 :: PathCost -> Int -> Int
part1 generated limit = getMin $ createPath [[]] generated limit

part2 :: PathCost -> Int -> Int
part2 generated limit = getMin $ createPath2 [[]] generated (limit + 1)

main :: IO ()
main = do
  input <- readFile "inputs/2016/day-24.txt"
  let linesInp = lines input
  let allNodes = getAllNodes input
  let lastNode = last allNodes
  let generated = generateCosts linesInp 0 lastNode

  putStrLn $ "Part 1 = " ++ show (part1 generated lastNode)
  putStrLn $ "Part 2 = " ++ show (part2 generated lastNode)