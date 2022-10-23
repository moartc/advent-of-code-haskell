import Data.List (isInfixOf)
import Data.List.Split ( splitOn )

input :: [Char]
input = "The first floor contains a strontium generator, a strontium-compatible microchip, a plutonium generator, and a plutonium-compatible microchip.\nThe second floor contains a thulium generator, a ruthenium generator, a ruthenium-compatible microchip, a curium generator, and a curium-compatible microchip.\nThe third floor contains a thulium-compatible microchip.\nThe fourth floor contains nothing relevant."

parseOnePart :: String -> String
parseOnePart s = firstPartAbb : [lastChar]
  where
    splitted = splitOn " " s
    firstPartAbb = head $ last $ init splitted
    lastChar = head $ last splitted

parseWholeLine :: String -> [String]
parseWholeLine line =
  if "nothing relevant" `isInfixOf` line
    then []
    else map parseOnePart splitted
  where
    dropped = drop 27 line
    withoutBeginning = splitOn " and a " dropped
    splitted = concatMap (splitOn " a ") withoutBeginning

parseInput :: String -> [[String]]
parseInput input = map parseWholeLine $ lines input

part1 :: String -> [String]
part1 = lines

main :: IO ()
main = do
  input <- readFile "inputs/day-11.txt"
  putStrLn $ "Part 1 = " ++ show (parseInput input)
