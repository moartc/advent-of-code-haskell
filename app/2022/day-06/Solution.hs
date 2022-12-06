import Data.Set (fromList)

solve :: String -> Int -> Int -> Int
solve string toTake counter
  | length (fromList (take toTake string)) == toTake = counter
  | otherwise = solve (drop 1 string) toTake $ counter + 1

main :: IO ()
main = do
  input <- readFile "inputs/2022/day-06.txt"
  putStrLn $ "Part 1 = " ++ show (solve input 4 4)
  putStrLn $ "Part 2 = " ++ show (solve input 14 14)
