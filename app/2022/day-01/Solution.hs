import Data.List ( sort )
import Data.List.Split (splitOn)

main :: IO ()
main = do
  input <- readFile "inputs/2022/day-01.txt"
  let sumArr = [sum (map (\ x -> (read x :: Int)) list) | list <- splitOn [""] $ lines input ] 
  putStrLn $ "Part 1 = " ++ show(maximum sumArr)
  putStrLn $ "Part 2 = " ++ show (sum $ take 3 $ reverse $ sort sumArr)