import Data.List (maximumBy, minimumBy, transpose)
import Data.Map (Map (), fromListWith, toList)
import Data.Ord (comparing)

frequencies :: String -> [(Char, Int)]
frequencies str = toList $ fromListWith (+) [(c, 1) | c <- str]

mostFrequent :: (Foldable t, Ord b) => t (a, b) -> a
mostFrequent tuple = fst $ maximumBy (comparing snd) tuple

lessCommon :: (Foldable t, Ord b) => t (a, b) -> a
lessCommon tuple = fst $ minimumBy (comparing snd) tuple

answer :: ([(Char, Int)] -> b) -> [[Char]] -> [b]
answer function = map (function . frequencies) . transpose

main :: IO ()
main = do
  input <- readFile "inputs/day-06.txt"
  putStrLn $ "Part 1 = " ++ show (answer mostFrequent $ lines input)
  putStrLn $ "Part 2 = " ++ show (answer lessCommon $ lines input)
