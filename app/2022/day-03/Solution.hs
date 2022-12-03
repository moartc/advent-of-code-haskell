import Data.Char
import Data.List (intersect)
import Data.List.Split (chunk, chunksOf)

numChar :: Char -> Int
numChar char = if isLower char then ord char - 96 else ord char - 38

part1 :: [String] -> Int
part1 input = sum $ map ((numChar . (\(l, r) -> head $ intersect l r)) . (\x -> splitAt (length x `div` 2) x)) input

part2 :: [String] -> Int
part2 input = sum $ map (\l -> numChar $ head $ foldr intersect (head l) $ tail l) $ chunksOf 3 input

main :: IO ()
main = do
  input <- readFile "inputs/2022/day-03.txt"
  putStrLn $ "Part 1 = " ++ show (part1 $ lines input)
  putStrLn $ "Part 2 = " ++ show (part2 $ lines input)
