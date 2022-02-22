{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Data.List (maximumBy, minimumBy, transpose)
import Data.Map (Map (), fromListWith, toList)
import Data.Ord (comparing)

indices :: String -> [Int]
indices string = [i | (c, i) <- zip string [0 ..], c == '[' || c == ']']

spl :: String -> Int -> [Int] -> Bool -> [String]
spl str _ [] _ = [str]
spl str y (x : xs) close = fst splitted : spl (snd splitted) next xs (not close)
  where
    next = if close then x + 1 else x
    splitted = splitAt toSplit str
    toSplit = if close then x - y + 1 else x - y

splitString :: String -> [String]
splitString str = spl str 0 (indices str) False

isAbba :: String -> Bool
isAbba [a, b, c, d] = a == d && b == c && a /= b
isAbba (a : b : c : d : xs) = (a == d && b == c && a /= b) || isAbba (b : c : d : xs)

inSquare :: [Char] -> Bool
inSquare str = head str == '[' && last str == ']'

isAnyNonBracketValid :: [String] -> Bool
isAnyNonBracketValid = any isNonBracketValid
  where
    isNonBracketValid str = not (inSquare str) && isAbba str

isAnyBracketInvalid :: [String] -> Bool
isAnyBracketInvalid = any isBracketInvalid
  where
    isBracketInvalid str = inSquare str && isAbba (init (tail str))

isStringValid :: [String] -> Bool
isStringValid xs = not (isAnyBracketInvalid xs) && isAnyNonBracketValid xs

countSupportTLS :: [String] -> Int
countSupportTLS = length . filter (isStringValid . splitString)

main :: IO ()
main = do
  input <- readFile "inputs/day-07.txt"
  putStrLn $ "Part 1 = " ++ show (countSupportTLS $ lines input)