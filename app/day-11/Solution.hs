import Data.List.Split
import Control.Arrow (Arrow(first))


input = "The first floor contains a hydrogen-compatible microchip and a lithium-compatible microchip.\nThe second floor contains a hydrogen generator.\nThe third floor contains a lithium generator.\nThe fourth floor contains nothing relevant."

str1="The first floor contains a hydrogen-compatible microchip and a lithium-compatible microchip."
str2="The second floor contains a hydrogen generator."
str3="The third floor contains a lithium generator."
str4="The fourth floor contains nothing relevant."

parseOnePart :: String -> String
parseOnePart s = firstPartAbb : [lastChar]
  where
    splitted = splitOn " " s
    firstPartAbb = head $ last $ init splitted
    lastChar = head $ last splitted

parseWholeLine :: String -> [String]
parseWholeLine line = arr
  where
    dropped = drop 27 line
    withoutBeginning = splitOn " and a " dropped
    splitted = concatMap (splitOn " a ") withoutBeginning
    arr = map parseOnePart splitted

parseInput :: String -> [[String]]
parseInput = map parseWholeLine . init . lines 

--parseInput :: String -> [[String]]
--parseInput s = dropWhile (a -> Bool) ([a])

--part1 :: String -> Int
--part1 input = takeWhile (a -> Bool) ([a])


main :: IO ()
main = do
  input <- readFile "inputs/day-11.txt"
  putStrLn "Part 1 = "
