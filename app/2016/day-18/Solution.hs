import Data.Map (Map, empty, insert, member, (!))

nextTile :: String -> Char
nextTile str
  | s0 == '^' && s1 == '^' && s2 == '.' = '^'
  | s0 == '.' && s1 == '^' && s2 == '^' = '^'
  | s0 == '^' && s1 == '.' && s2 == '.' = '^'
  | s0 == '.' && s1 == '.' && s2 == '^' = '^'
  | otherwise = '.'
  where
    s0 = head str
    s1 = str !! 1
    s2 = str !! 2

get3For :: String -> Int -> String
get3For prev idx = [prev !! idx] ++ [prev !! (idx + 1)] ++ [prev !! (idx + 2)]

nextRow :: String -> String
nextRow row = toRet
  where
    toRet = if Data.Map.member row rows then rows ! row else insert row newRow rows ! row
    fixed = "." ++ row ++ "."
    newRow = [nextTile prev3 | i <- [0 .. length row -1], let prev3 = get3For fixed i]
    rows :: Map String String
    rows = empty

count :: Int -> String -> Int -> Int
count 1 inp ctr = length (filter (== '.') inp) + ctr
count x inp ctr = count (x -1) (nextRow inp) updatedCtr
  where
    updatedCtr = ctr + length (filter (== '.') inp)

part2 :: String -> Int -> Int
part2 inp num = count num inp 0

main :: IO ()
main = do
  input <- readFile "inputs/2016/day-18.txt"
  putStrLn $ "Part 1 = " ++ show (part2 input 40)
  putStrLn $ "Part 2 = " ++ show (part2 input 400000)
