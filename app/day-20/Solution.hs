import Data.List

parseRow :: String -> (Int, Int)
parseRow row = (read (takeWhile (/= '-') row) :: Int, read (tail $ dropWhile (/= '-') row) :: Int)

parseInp :: String -> [(Int, Int)]
parseInp inp = map parseRow (lines inp)

blackListed :: Int -> [(Int, Int)] -> Bool
blackListed val = any (\(l, r) -> l <= val && r >= val)

getNewMax :: Int -> [(Int, Int)] -> Int
getNewMax val sorted = snd (head $ filter (\(f, s) -> f <= val && s >= val) sorted) + 1

solvePart1 :: Int -> [(Int, Int)] -> Int
solvePart1 val sortedList
  | not $ blackListed val sortedList = val
  | otherwise = solvePart1 (getNewMax val sortedList) sortedList

solvePart2 :: Int -> Int -> [(Int, Int)] -> Int
solvePart2 val ctr sortedList
  | val > 4294967295 = ctr
  | not $ blackListed val sortedList = solvePart2 (val + 1) (ctr + 1) sortedList
  | otherwise = solvePart2 (getNewMax val sortedList) ctr sortedList

part1 :: String -> Int
part1 inp = ans
  where
    list = sort $ parseInp inp
    ans = solvePart1 0 list

part2 :: String -> Int
part2 inp = ans
  where
    list = sort $ parseInp inp
    ans = solvePart2 0 0 list

main :: IO ()
main = do
  input <- readFile "inputs/day-20.txt"
  putStrLn $ "Part 1 = " ++ show (part1 input)
  putStrLn $ "Part 2 = " ++ show (part2 input)
