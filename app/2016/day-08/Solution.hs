{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Data.Function ( on )
import Data.List ( isInfixOf, sortBy, transpose )

main :: IO ()
main = do
  input <- readFile "inputs/2016/day-08.txt"
  let screen = createScreen 50 6
  putStrLn $ "Part 1 = " ++ show (countLit (foldl action screen (lines input)))
  putStrLn  "Part 2:" 
  mapM_ print (foldl action screen (lines input))

createScreen :: Int -> Int -> [String]
createScreen x y = replicate y (replicate x '.')

rect :: [String] -> (Int, Int) -> [String]
rect str (xr, yr) = [(if y < yr then change string else string) | (string, y) <- zip str [0 ..]]
  where
    change s = [if x < xr then '#' else c | (c, x) <- zip s [0 ..]]

newIndex :: (Char, Int) -> Int -> Int -> (Char, Int)
newIndex (c, i) by length = (c, (i + by) `mod` length)

rotate :: String -> Int -> [Char]
rotate line by = sortTuple [newIndex (c, i) by (length line) | (c, i) <- zip line [0 ..]]

sortTuple :: Ord a1 => [(a2, a1)] -> [a2]
sortTuple input = [c | (c, i) <- sortBy (compare `on` snd) input]

rotateRow :: [String] -> (Int, Int) -> [String]
rotateRow arr (row, by) = replace row (rotate (arr !! row) by) arr

rotateColumn :: [String] -> (Int, Int) -> [String]
rotateColumn arr t = transpose $ rotateRow (transpose arr) t

replace :: Int -> a -> [a] -> [a]
replace pos newVal list = take pos list ++ newVal : drop (pos + 1) list

rectArgs :: [Char] -> (Int, Int)
rectArgs str = (read (takeWhile (/= 'x') dropped) :: Int, read $ reverse $ takeWhile (/= 'x') $ reverse dropped :: Int)
  where
    dropped = drop 5 str

rotateArgs :: [Char] -> Int -> (Int, Int)
rotateArgs str toDrop = (read (takeWhile (/= ' ') dropped) :: Int, read $ reverse $ takeWhile (/= ' ') $ reverse dropped :: Int)
  where
    dropped = drop toDrop str

action :: [String] -> String -> [String]
action arr command
  | "rect" `isInfixOf` command = rect arr $ rectArgs command
  | "rotate column" `isInfixOf` command = rotateColumn arr $ rotateArgs command 16
  | otherwise = rotateRow arr $ rotateArgs command 13

countLit :: [String] -> Int
countLit arr = sum [length $ filter (=='#') s | s <- arr]