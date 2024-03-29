import Data.Digest.Pure.MD5 (md5)
import Data.String (IsString (fromString))

data State = State {pos :: (Int, Int), code :: String, answer :: String} deriving (Show)

hashStr :: String -> String
hashStr = show . md5 . fromString

open :: [Char]
open = ['b', 'c', 'd', 'e', 'f']

getDoors :: String -> (Int, Int) -> [Char]
getDoors input pos = toRet
  where
    u = (input !! 0) `elem` open && fst pos > 0
    d = (input !! 1) `elem` open && fst pos < 3
    l = (input !! 2) `elem` open && snd pos > 0
    r = (input !! 3) `elem` open && snd pos < 3
    toRet = ['U' | u] ++ ['D' | d] ++ ['L' | l] ++ ['R' | r]

shortest :: [String] -> String
shortest list = snd $ minimum [(length x, x) | x <- list]

updatePos :: (Int, Int) -> Char -> (Int, Int)
updatePos current doors
  | doors == 'U' = (fst current - 1, snd current)
  | doors == 'D' = (fst current + 1, snd current)
  | doors == 'L' = (fst current, snd current -1)
  | otherwise = (fst current, snd current + 1)

doStep :: State -> State
doStep state
  | length (answer state) < length (code state) = state
  | isPosFinal && length (answer state) > length (code state) = State (pos state) (code state) (code state)
  | otherwise = foldl (\x y1 -> doStep (State (updatePos p y1) (code state ++ [y1]) (answer x))) state [d | d <- getDoors (hashStr $ code state) p]
  where
    isPosFinal = fst p == 3 && snd p == 3
    p = pos state

doStep2 :: State -> State
doStep2 state
  | isPosFinal && length (answer state) < length (code state) = State (pos state) (code state) (code state)
  | isPosFinal = state
  | otherwise = foldl (\x y1 -> doStep2 (State (updatePos p y1) (code state ++ [y1]) (answer x))) state [d | d <- getDoors (hashStr $ code state) p]
  where
    isPosFinal = fst p == 3 && snd p == 3
    p = pos state    

part1 :: String -> String
part1 input = drop (length input) $ answer $ doStep2 (State (0, 0) input "short answer")

part2 :: String -> Int
part2 input = length $ drop (length input) $ answer $ doStep2 (State (0, 0) input "short answer")

main :: IO ()
main = do
  let input = "vwbaicqe"
  putStrLn $ "Part 1 = " ++ show (part1 input)
  putStrLn $ "Part 2 = " ++ show (part2 input)
