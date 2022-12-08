import Data.Char (digitToInt)

parse :: String -> [[Int]]
parse inp = grid
  where
    rows = lines inp
    grid = [map digitToInt row | row <- rows]

isTreeVisible :: [[Int]] -> Int -> Int -> Bool
isTreeVisible grid y x 
  | y == 0 = True
  | y == length grid -1 = True
  | x == 0 = True
  | x == length (head grid) -1 = True
  | otherwise = up || down || left || right
  where
    up = all (\v -> grid !! y !! x > v) [row !! x | (row, idx) <- zip grid [0 ..], idx < y]
    down = all (\v -> grid !! y !! x > v) [row !! x | (row, idx) <- zip grid [0 ..], idx > y]
    left = all (\v -> v < grid !! y !! x) $ [row | (row, idx) <- zip (grid !! y) [0 ..], idx < x]
    right = all (\v -> v < grid !! y !! x) [row | (row, idx) <- zip (grid !! y) [0 ..], idx > x]

part1 :: String -> Int
part1 inp = length $ filter (== True) $ map (uncurry (isTreeVisible parsed)) allPairs
  where
    parsed = parse inp
    allPairs = [(i, j) | i <- [0 .. length parsed -1], j <- [0 .. length (head parsed) -1]]

countTree :: [Int] -> Int -> Int -> Int
countTree (x : xs) value counter
  | x >= value = counter
  | otherwise = countTree xs value (counter + 1)
countTree [] _ counter = counter -1

countVisible :: [[Int]] -> Int -> Int -> Int
countVisible grid y x = up * down * left * right
  where
    up = countTree arrUp (grid !! y !! x) 1
    arrUp = reverse [row !! x | (row, idx) <- zip grid [0 ..], idx < y]
    down = countTree arrDown (grid !! y !! x) 1
    arrDown = [row !! x | (row, idx) <- zip grid [0 ..], idx > y]
    left = countTree arrLeft (grid !! y !! x) 1
    arrLeft = reverse [row | (row, idx) <- zip (grid !! y) [0 ..], idx < x]
    right = countTree arrRight (grid !! y !! x) 1
    arrRight = [row | (row, idx) <- zip (grid !! y) [0 ..], idx > x]

part2 :: String -> Int
part2 inp = maximum $ map (uncurry (countVisible parsed)) allPairs
  where
    parsed = parse inp
    allPairs = [(i, j) | i <- [1 .. length parsed -2], j <- [1 .. length (head parsed) -2]]

main :: IO ()
main = do
  input <- readFile "inputs/2022/day-08.txt"
  putStrLn $ "Part 1 = " ++ show (part1 input)
  putStrLn $ "Part 2 = " ++ show (part2 input)
