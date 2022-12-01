import Data.List ( sort )

inpToList :: [String] ->Int-> [Int] -> [Int]
inpToList (x:xs) prev list
  | x == "" = inpToList xs 0 list ++ [prev]
  | otherwise = inpToList xs (prev + (read x :: Int)) list    
inpToList [] _ list = list

part1 :: [String] -> Int
part1 inp = maximum $ inpToList inp 0 [] 

part2 :: [String] -> Int
part2 inp = sum $ take 3 $ reverse $ sort $ inpToList inp 0 []   
    

main :: IO ()
main = do
  input <- readFile "inputs/2022/day-01.txt"  
  putStrLn $ "Part 1 = " ++ show(part1 $ lines input)
  putStrLn $ "Part 2 = " ++ show (part2 $ lines input)