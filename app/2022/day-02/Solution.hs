import Data.Map ((!))
import qualified Data.Map as Map
import Data.Map.Strict (Map)

gameResult :: Map (Char, Char) Int
gameResult =
  Map.fromList
    [ (('A', 'X'), 3),
      (('A', 'Y'), 6),
      (('A', 'Z'), 0),
      (('B', 'X'), 0),
      (('B', 'Y'), 3),
      (('B', 'Z'), 6),
      (('C', 'X'), 6),
      (('C', 'Y'), 0),
      (('C', 'Z'), 3)
    ]

p2Map :: (Char, Char) -> Int
p2Map (op, result) = resPoint + pointsForChoice ! snd (head $ filter (\(x, y) -> x == op) $ Map.keys $ Map.filter (== resPoint) gameResult)
  where
    resPoint
      | result == 'X' = 0
      | result == 'Y' = 3
      | otherwise = 6

pointsForChoice :: Map Char Int
pointsForChoice = Map.fromList [('X', 1), ('Y', 2), ('Z', 3)]

part1 :: [String] -> Int
part1 input = sum $ [(\(op, me) -> gameResult ! (op, me) + (pointsForChoice ! me)) (head line, last line) | line <- input]

part2 :: [String] -> Int
part2 input = sum $ [p2Map (head line, last line) | line <- input]

main :: IO ()
main = do
  input <- readFile "inputs/2022/day-02.txt"
  putStrLn $ "Part 1 = " ++ show (part1 $ lines input)
  putStrLn $ "Part 2 = " ++ show (part2 $ lines input)
