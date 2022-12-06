import Data.List (transpose)
import Data.List.Split (splitOn)
import Data.List.Split.Internals (splitWhen)

replace :: Int -> a -> [a] -> [a]
replace pos newVal list = take pos list ++ newVal : drop (pos + 1) list

charsFromLine :: String -> String
charsFromLine inp = [c | (c, i) <- zip inp [0 ..], (i -1) `mod` 4 == 0]

parseInput :: String -> ([String], [String])
parseInput inp = (mapToRet, splitted !! 1)
  where
    splitted = splitWhen (== "") $ lines inp
    mapToRet = map (takeWhile (/= ' ') . reverse) $ transpose $ map charsFromLine $ init $ head splitted

move :: [String] -> String -> Bool -> [String]
move grid command isP2 = replace from newFrom $ replace to newTo grid
  where
    splitted = splitOn " " command
    move = read $ splitted !! 1 :: Int
    from = (read $ splitted !! 3 :: Int) -1
    to = (read $ splitted !! 5 :: Int) - 1
    toMove = take move $ reverse $ grid !! from
    newFrom = take (length (grid !! from) - move) $ grid !! from
    newTo = (grid !! to) ++ (if isP2 then reverse toMove else toMove)

solve :: [String] -> [String] -> Bool -> String
solve grid [] p2 = map last $ filter (/= "") grid
solve grid (m : ms) p2 = solve (move grid m p2) ms p2

main :: IO ()
main = do
  input <- readFile "inputs/2022/day-05.txt"
  let (grid, moves) = parseInput input
  putStrLn $ "Part 1 = " ++ show (solve grid moves False)
  putStrLn $ "Part 2 = " ++ show (solve grid moves True)
