import Data.Char (digitToInt, intToDigit)
import Data.List.Split (splitOn)
import Data.Map (Map, empty, insert, member, (!))

data Node = Node {x :: Int, y :: Int, size :: Int, used :: Int, avail :: Int} deriving (Show, Eq, Ord)

trim :: String -> String
trim = unwords . words

parseNode :: String -> Node
parseNode inp = Node nodeX nodeY nodeSize nodeUsed nodeAvail
  where
    toDrop = "/dev/grid/node-x"
    dropped = drop (length toDrop) inp
    splitted = map trim $ filter (/= "") $ splitOn "  " dropped
    nodes = head splitted
    nodeX = read $ takeWhile (/= '-') nodes :: Int
    nodeY = read $ reverse $ takeWhile (/= 'y') $ reverse nodes :: Int
    nodeSize = read $ init $ splitted !! 1 :: Int
    nodeUsed = read $ init $ splitted !! 2 :: Int
    nodeAvail = read $ init $ splitted !! 3 :: Int

parseInput :: String -> [Node]
parseInput inp = map parseNode $ drop 2 $ lines inp

validPair :: Node -> Node -> Bool
validPair node1 node2 = ans
  where
    ans = notEmpty && diffNodes && dataFit
    notEmpty = used node1 /= 0
    diffNodes = node1 /= node2
    dataFit = used node1 <= avail node2

validPairs :: [Node] -> Int
validPairs nodes = countValid
  where
    pairs = [(n1, n2) | (n1, i) <- zip nodes [0 ..], (n2, j) <- zip nodes [0 ..], i /= j]
    countValid = length $ filter (== True) [validPair n1 n2 | (n1, n2) <- pairs]

part1 :: String -> Int
part1 inp = ans
  where
    ans = validPairs nodes
    nodes = parseInput inp

canMove :: Int -> Int -> Int -> Int -> Bool
canMove x1 y1 x2 y2 = (xDiff == 1 && yDiff == 0) || (xDiff == 0 && yDiff == 1)
  where
    xDiff = abs $ x1 - x2
    yDiff = y1 - y2

toMove :: [Node] -> [Node]
toMove all = filter (\n -> used n <= avail empty && canMove (x empty) (y empty) (x n) (y n) && size n < 101) all
  where
    empty = head $ filter (\n -> used n == 0) all

move :: [Node] -> Node -> [Node]
move all toMove = [if n == toMove then n {used = 0, avail = size n} else if used n == 0 then n {used = used toMove, avail = size n - used toMove} else n | n <- all]

getEmpty :: [Node] -> (Int, Int)
getEmpty nodes = (x empty, y empty)
  where
    empty = head $ filter (\n -> used n == 0) nodes

leftOnGoal :: Int -> [Node] -> Int -> Map (Int, Int) Int -> Map (Int, Int) Int
leftOnGoal pos nodes counter cache
  | shouldExit = cache
  | emptyX == pos && emptyY == 0 = updatedCache
  | otherwise = foldl (\c node -> leftOnGoal pos (move nodes node) (counter + 1) c) updatedCache $ toMove nodes
  where
    currentMin = if member (emptyX, emptyY) cache then cache ! (emptyX, emptyY) else 999999999
    shouldExit = currentMin <= counter
    updatedCache = insert (emptyX, emptyY) counter cache
    (emptyX, emptyY) = getEmpty nodes

moveGoalTo00 :: ((Int, Int), Int) -> Int
moveGoalTo00 ((goalX, emptyX), counter)
  | goalX == 0 = counter
  | otherwise = moveGoalTo00 $ moveGoalOneStepWithEmptyOnRight counter goalX emptyX

moveGoalOneStepWithEmptyOnRight :: Int -> Int -> Int -> ((Int, Int), Int)
moveGoalOneStepWithEmptyOnRight counter goalX emptyX = ((goalX -1, emptyX -1), counter + 5)

part2 :: String -> Int
part2 input = ((rightBound - 1) * 5) + leftOnGoalRes + 1
  where
    nodes = parseInput input
    rightBound = maximum $ map x nodes
    part2Nodes = filter (\n -> y n <= snd (getEmpty nodes)) nodes
    leftOnGoalState = leftOnGoal (rightBound -1) nodes 0 empty
    leftOnGoalRes = leftOnGoalState ! (rightBound -1, 0)

main :: IO ()
main = do
  input <- readFile "inputs/day-22.txt"
  putStrLn $ "Part 1 = " ++ show (part1 input)
  putStrLn $ "Part 2 = " ++ show (part2 input)
