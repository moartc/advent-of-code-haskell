import Data.Char (chr, isDigit, ord)
import Data.List (elemIndex, findIndex, isPrefixOf, sortBy)
import Data.Map (Map (), fromListWith, toList)
import Data.Maybe (fromMaybe)

data Line = Line String Int String deriving (Show)

parse :: String -> Line
parse input = Line (init name) sectorId checksum
  where
    sectorId = read orderString
    checksum = init $ tail checksumWithBrackets
    (name, orderWithCheckSum) = splitAt (fromMaybe 0 $ findIndex isDigit input) input
    (orderString, checksumWithBrackets) = splitAt (fromMaybe 0 $ elemIndex '[' orderWithCheckSum) orderWithCheckSum

getValidChecksum :: String -> String
getValidChecksum input = take 5 [x | (x, _) <- sorted]
  where
    sorted = sortBy (\(_, a) (_, b) -> compare b a) map
    map = toList $ fromListWith (+) [(c, 1) | c <- input, c /= '-']

part1 :: String -> Int
part1 input = sum [sId | Line name sId checksum <- parsed, getValidChecksum name == checksum]
  where
    parsed = map parse (lines input)

-- PART 2 --

getRealName :: Int -> String -> String
getRealName sectorId [] = []
getRealName sectorId (x : xs) = toReturn : getRealName sectorId xs
  where
    toReturn = if x /= '-' then returnNext x sectorId else ' '

returnNext :: Char -> Int -> Char
returnNext c n = chr (((n + ord c - ord 'a') `mod` 26) + ord 'a')

part2 :: String -> Int
part2 input = head [sId | Line name sId checksum <- parsed, "northpole" `isPrefixOf` getRealName sId name]
  where
    parsed = map parse (lines input)

main :: IO ()
main = do
  input <- readFile "inputs/2016/day-04.txt"
  putStrLn $ "Part 1 = " ++ show (part1 input)
  putStrLn $ "Part 2 = " ++ show (part2 input)