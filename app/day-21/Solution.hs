import Data.Char (digitToInt)
import Data.List (elemIndex, isPrefixOf)
import Data.Maybe (fromMaybe)

data Op = SwapP Int Int | SwapL Char Char | RotateL Int | RotateR Int | RotateB Char | Reverse Int Int | Move Int Int deriving (Show)

rotateBstr = "rotate based on position of letter "
rotateLstr = "rotate left "
rotateRstr = "rotate right "
swapPstr = "swap position "
swapLstr = "swap letter "
reverseStr = "reverse positions "
moveStr = "move position "

parseLine :: String -> Op
parseLine inp
  | rotateBstr `isPrefixOf` inp = RotateB $ last inp
  | rotateLstr `isPrefixOf` inp = RotateL (digitToInt (inp !! max 0 (length rotateLstr)))
  | rotateRstr `isPrefixOf` inp = RotateR (digitToInt (inp !! max 0 (length rotateRstr)))
  | swapPstr `isPrefixOf` inp = SwapP (digitToInt $ inp !! max 0 (length swapPstr)) (digitToInt $ last inp)
  | swapLstr `isPrefixOf` inp = SwapL (inp !! max 0 (length swapLstr)) (last inp)
  | reverseStr `isPrefixOf` inp = Reverse (digitToInt (inp !! max 0 (length reverseStr))) (digitToInt $ last inp)
  | otherwise = Move (digitToInt $ inp !! max 0 (length moveStr)) (digitToInt $ last inp)

parseInput :: [String] -> [Op]
parseInput = map parseLine

doOp :: Op -> String -> String
doOp (RotateL x) str = rotateL str x
doOp (RotateR x) str = rotateR str x
doOp (RotateB x) str = rotateB str x
doOp (SwapP x y) str = [if i == x then str !! y else if i == y then str !! x else c | (c, i) <- zip str [0 ..]]
doOp (SwapL x y) str = [if c == x then y else if c == y then x else c | c <- str]
doOp (Reverse x y) str = take x str ++ reverse (take (y - x + 1) $ drop x str) ++ drop (y + 1) str
doOp (Move x y) str = move str x y

rotateB :: String -> Char -> String
rotateB str char = rotateR str $ 1 + idx + if idx >= 4 then 1 else 0
  where
    idx = fromMaybe 0 $ elemIndex char str

rotateR :: String -> Int -> String
rotateR str 0 = str
rotateR str n = if n == 1 then rot else rotateR rot (n -1)
  where
    rot = last str : init str

rotateL :: String -> Int -> String
rotateL str 0 = str
rotateL str n = if n == 1 then rot else rotateL rot (n -1)
  where
    rot = tail str ++ [head str]

move :: String -> Int -> Int -> String
move str x y = concat [if i == y then (if y > x then c : [charToMove] else charToMove : [c]) else [c] | (c, i) <- zip str [0 ..], i /= x]
  where
    charToMove = str !! x

part1 :: String -> String -> String
part1 inp str = foldl (flip doOp) str $ parseInput $ lines inp

part2 :: String -> String -> String
part2 inp str = foldl (flip revOp) str $ reverse $ parseInput $ lines inp

revOp :: Op -> String -> String
revOp (RotateL x) str = doOp (RotateR x) str
revOp (RotateR x) str = doOp (RotateL x) str
revOp (RotateB x) str
  | idx == 1 = doOp (RotateL 1) str
  | idx == 3 = doOp (RotateL 2) str
  | idx == 5 = doOp (RotateL 3) str
  | idx == 7 = doOp (RotateL 4) str
  | idx == 2 = doOp (RotateR 2) str
  | idx == 4 = doOp (RotateR 1) str
  | idx == 6 = str
  | otherwise = doOp (RotateL 1) str
  where
    idx = fromMaybe 0 $ elemIndex x str
revOp (SwapP x y) str = doOp (SwapP y x) str
revOp (SwapL x y) str = doOp (SwapL y x) str
revOp (Reverse x y) str = doOp (Reverse x y) str
revOp (Move x y) str = doOp (Move y x) str

main :: IO ()
main = do
  input <- readFile "inputs/day-21.txt"
  putStrLn $ "Part 1 = " ++ show (part1 input "abcdefgh")
  putStrLn $ "Part 2 = " ++ show (part2 input "fbgdceah")
