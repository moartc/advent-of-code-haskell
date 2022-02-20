import Data.List (transpose)
import Data.List.Split (chunksOf)

data Triple = Triple Int Int Int deriving (Show)

parse :: String -> [Int]
parse line = [read x :: Int | x <- words line]

toTriple :: [Int] -> Triple
toTriple xs = Triple (head xs) (xs !! 1) (xs !! 2)

isTriangle :: Triple -> Bool
isTriangle (Triple a b c) = a + b > c && a + c > b && b + c > a

triples :: String -> [Triple]
triples = map (toTriple . parse) . lines

countTriangles :: [Triple] -> Int
countTriangles = length . filter isTriangle

part1 :: String -> Int
part1 = countTriangles . triples

triplesPart2 :: String -> [Triple]
triplesPart2 input = map toTriple (concat [chunksOf 3 x | x <- transpose [[x, y, z] | [x, y, z] <- chunksOf 3 (parse input)]])

part2 :: String -> Int
part2 = countTriangles . triplesPart2

main :: IO ()
main = do
  input <- readFile "inputs/day-03.txt"
  putStrLn $ "Part 1 = " ++ show (part1 input)
  putStrLn $ "Part 2 = " ++ show (part2 input)