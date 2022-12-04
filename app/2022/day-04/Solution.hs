import Data.List.Split (splitOn)

part1 :: [String] -> Int
part1 input = length $ filter(==True) [all (`elem` right) left || all (`elem` left) right | (left, right) <- map range input]

part2 :: [String] -> Int
part2 input = length $ filter(==True) [any (`elem` right) left || any (`elem` left) right | (left, right) <- map range input]

range :: String -> ([Int], [Int])
range str = ([read $ head l :: Int .. read (l !! 1) :: Int], [read $ head r :: Int .. read (r !! 1) :: Int])
  where
    arr = splitOn "," str
    l = splitOn "-" $ head arr
    r = splitOn "-" $ arr !! 1

main :: IO ()
main = do
  input <- readFile "inputs/2022/day-04.txt"
  putStrLn $ "Part 1 = " ++ show (part1 $ lines input)
  putStrLn $ "Part 2 = " ++ show (part2 $ lines input)
