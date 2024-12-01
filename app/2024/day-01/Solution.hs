import Data.List (sort)
import qualified Data.Map as Map

listInts :: String -> [Int]
listInts input = map read $ words input :: [Int]

toPair :: [b] -> (b, b)
toPair [x, y] = (x, y)

part1 :: [String] -> Int
part1 input = sum $ map abs $ zipWith (-) left right
  where
    pairOfLists = unzip $ map (toPair . listInts) input
    left = sort $ fst pairOfLists
    right = sort $ snd pairOfLists

frequency :: [Int] -> Map.Map Int Int
frequency = foldr (\x -> Map.insertWith (+) x 1) Map.empty

part2 :: [String] -> Int
part2 input = sum result
  where
    pairOfLists = unzip $ map (toPair . listInts) input
    freq = frequency $ snd pairOfLists
    firstList = fst pairOfLists
    result = map (uncurry (*) . (\x -> (x, Map.findWithDefault 0 x freq))) firstList

main :: IO ()
main = do
  input <- readFile "inputs/2024/day-01.txt"
  putStrLn $ "Part 1: " ++ show (part1 $ lines input)
  putStrLn $ "Part 2: " ++ show (part2 $ lines input)