import Data.Char (ord)
import Data.Map (Map, empty, fromList, insert, (!))

parseInput :: String -> [[Char]]
parseInput = lines

part1 :: String -> Int
part1 inp = getResult grid s e
  where
    grid = lines inp
    s = getPos 'S'
    e = getPos 'E'
    startEnd = take 2 [(c, y, x) | (line, y) <- zip grid [0 ..], (c, x) <- zip line [0 ..], c == 'S' || c == 'E']
    getPos char = head $ map (\(_, y, x) -> (y, x)) $ filter (\(x, _, _) -> x == char) startEnd

part2 :: String -> Int
part2 inp = minimum [getResult grid start e | start <- allA]
  where
    grid = lines inp
    allA = [(y, x) | (line, y) <- zip grid [0 ..], (c, x) <- zip line [0 ..], c == 'a']
    e = getPos 'E'
    startEnd = take 2 [(c, y, x) | (line, y) <- zip grid [0 ..], (c, x) <- zip line [0 ..], c == 'S' || c == 'E']
    getPos char = head $ map (\(_, y, x) -> (y, x)) $ filter (\(x, _, _) -> x == char) startEnd

getResult :: [String] -> (Int, Int) -> (Int, Int) -> Int
getResult grid start = bfs changedGrid [start] updatedWithZero
  where
    updatedWithZero = insert start 0 infinitDist
    infinitDist = foldl (\m p -> insert p maxBound m) empty [(y, x) | x <- [0 .. length (head grid) -1], y <- [0 .. length grid -1]]
    changedGrid = [[if c == 'E' then 'z' else if c == 'S' then 'a' else c | c <- line] | line <- grid]

bfs :: [[Char]] -> [(Int, Int)] -> Map (Int, Int) Int -> (Int, Int) -> Int
bfs grid queue bestDistance end
  | null queue = maxBound
  | currentPos == end = currentDistance
  | otherwise = bfs grid (restQueue ++ next) updatedDist end
  where
    currentPos = head queue
    restQueue = tail queue
    currentChar = grid !! fst currentPos !! snd currentPos
    currentDistance = bestDistance ! currentPos
    next =
      [ nextPos
        | p <- pos,
          let nextPos = add currentPos p,
          let nextChar = grid !! fst nextPos !! snd nextPos,
          isPosValid nextPos
            && canVisit currentChar nextChar
            && bestDistance ! nextPos > currentDistance + 1
      ]
    updatedDist = foldl (\m p -> insert p (currentDistance + 1) m) bestDistance next
    canVisit :: Char -> Char -> Bool
    canVisit f n = ord n - ord f <= 1
    isPosValid :: (Int, Int) -> Bool
    isPosValid (y, x) =
      y >= 0 && y < length grid && x >= 0 && x < length (head grid)

pos :: [(Int, Int)]
pos = [(-1, 0), (1, 0), (0, -1), (0, 1)]

add :: (Int, Int) -> (Int, Int) -> (Int, Int)
add (y1, x1) (y2, x2) = (y1 + y2, x1 + x2)

main :: IO ()
main = do
  input <- readFile "inputs/2022/day-12.txt"
  putStrLn $ "Part 1 = " ++ show (part1 input)
  putStrLn $ "Part 2 = " ++ show (part2 input)
