import qualified Data.Bifunctor
import Data.Char (isDigit)
import Data.List (elemIndex, sort)
import Data.List.Split (chunksOf)
import Data.Maybe (fromJust)

data Arr = Int Int | Arr [Arr] deriving (Show, Read, Eq)

parseInput :: String -> Arr
parseInput str = read $ enhanceString str :: Arr

enhanceString :: String -> String
enhanceString (x : y : xs)
  | x == '[' && isDigit y = "Arr[Int " ++ [y] ++ enhanceString xs
  | x == ',' && isDigit y = [x] ++ " Int " ++ [y] ++ enhanceString xs
  | x == '[' = "Arr[" ++ enhanceString (y : xs)
  | otherwise = x : enhanceString (y : xs)
enhanceString [x] = [x]
enhanceString [] = []

instance Ord Arr where
  (Int x) `compare` (Int y) = compare x y
  (Arr x) `compare` r@(Int y) = compare x [r]
  l@(Int x) `compare` (Arr y) = compare [l] y
  (Arr x) `compare` (Arr y) = compare x y

part1 :: String -> Int
part1 inp = sum $ map snd $ filter (\x -> fst x == LT) [(a1 `compare` a2, idx) | ((a1, a2), idx) <- zip parsedInp [1 ..]]
  where
    parsedInp = map (Data.Bifunctor.bimap parseInput parseInput . (\q -> (head q, q !! 1))) (chunksOf 3 $ lines inp)

part2 :: String -> Int
part2 inp = (idx1 + 1) * (idx2 + 1)
  where
    sorted = sort $ map parseInput $ concatMap (take 2) (chunksOf 3 $ lines inp) ++ ["[[2]]"] ++ ["[[6]]"]
    idx1 = fromJust $ elemIndex (parseInput "[[2]]") sorted
    idx2 = fromJust $ elemIndex (parseInput "[[6]]") sorted

main :: IO ()
main = do
  input <- readFile "inputs/2022/day-13.txt"
  putStrLn $ "Part 1 = " ++ show (part1 input)
  putStrLn $ "Part 2 = " ++ show (part2 input)
