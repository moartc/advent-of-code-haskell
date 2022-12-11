import Data.Char (digitToInt, isDigit)
import Data.Either (rights)
import Data.List (sort)
import Data.List.Split (splitOn)
import Data.Monoid (First (First))
import Debug.Trace

debug = flip trace

data Monkey = Monkey {items :: [Int], op :: Char, opRightSide :: Either Int String, divisibleBy :: Int, ifTrue :: Int, ifFalse :: Int} deriving (Show)

addItem :: Monkey -> Int -> Monkey
addItem m i = m {items = items m ++ [i]}

resetItems :: Monkey -> Monkey
resetItems m = m {items = []}

inp = "Monkey 0:\n  Starting items: 79, 98\n  Operation: new = old * 19\n  Test: divisible by 23\n    If true: throw to monkey 2\n    If false: throw to monkey 3\n\nMonkey 1:\n  Starting items: 54, 65, 75, 74\n  Operation: new = old + 6\n  Test: divisible by 19\n    If true: throw to monkey 2\n    If false: throw to monkey 0\n\nMonkey 2:\n  Starting items: 79, 60, 97\n  Operation: new = old * old\n  Test: divisible by 13\n    If true: throw to monkey 1\n    If false: throw to monkey 3\n\nMonkey 3:\n  Starting items: 74\n  Operation: new = old + 3\n  Test: divisible by 17\n    If true: throw to monkey 0\n    If false: throw to monkey 1\n"

t1 = parseInput inp

newVal :: Int -> Char -> Either Int String -> Int
newVal int char (Left newVal) = if char == '*' then int * newVal else int + newVal
newVal int char (Right _) = if char == '*' then int * int else int + int

type RoundState = ([Int], [Monkey])

oneMonkeyRound :: RoundState -> Int -> RoundState
oneMonkeyRound (insp, ms) monkeyIdx = (updatedInsp, fst x)
  where
    currentMonkeyInsp = insp !! monkeyIdx
    x = monkeyRound ms monkeyIdx
    updatedInsp = replace monkeyIdx (currentMonkeyInsp + snd x) insp

part1 :: String -> Int
part1 inp = product $ take 2 $ reverse $ sort $ fst finalRS
  where
    monkeys = parseInput inp
    initRoundState = (replicate (length monkeys) 0, monkeys)
    finalRS = foldl (\x y -> allMonkeysRound x 0) initRoundState [1 .. 20]
    

allMonkeysRound :: RoundState -> Int -> RoundState
allMonkeysRound rs idx
  | idx == ms = rs
  | otherwise = allMonkeysRound newState (idx + 1)
  where
    ms = length $ snd rs
    newState = oneMonkeyRound rs idx

monkeyRound :: [Monkey] -> Int -> ([Monkey], Int)
monkeyRound monkeys idx = (replace idx resetMonk firstUpdate, length $ items monk) -- `debug` ("return " ++ show (inspToRet))
  where
    monk = monkeys !! idx    
    firstUpdate = updateMonkeys monkeys [handleItem monk item | item <- items monk]  
    resetMonk = resetItems monk    

updateMonkeys :: [Monkey] -> [(Int, Int)] -> [Monkey]
updateMonkeys all ((toReplace, newValue) : us) = updateMonkeys replaced us
  where
    monkeyToReplace = all !! toReplace
    updated = addItem monkeyToReplace newValue
    replaced = replace toReplace updated all
updateMonkeys all [] = all

handleItem :: Monkey -> Int -> (Int, Int)
handleItem monkey itemVal = (throwTo, valToThrow)
  where
    newValue = newVal itemVal (op monkey) (opRightSide monkey)
    valToThrow = newValue `div` 3
    throwTo = if valToThrow `mod` divisibleBy monkey == 0 then ifTrue monkey else ifFalse monkey

parseInput :: String -> [Monkey]
parseInput input = map parseMonkey $ splitOn [""] $ lines input

parseMonkey :: [String] -> Monkey
parseMonkey input = Monkey items operator rightVal divisibleBy ifTrue ifFalse
  where
    items = map (\x -> read x :: Int) $ splitOn ", " $ dropWhile (not . isDigit) $ input !! 1
    operation = last $ splitOn " = " $ input !! 2
    operator = operation !! 4
    rightSide = last $ splitOn " " operation
    rightVal = if isDigit (head rightSide) then Left (read rightSide :: Int) else Right rightSide
    divisibleBy = read $ dropWhile (not . isDigit) $ input !! 3 :: Int
    ifTrue = digitToInt $ last $ input !! 4
    ifFalse = digitToInt $ last $ input !! 5

main :: IO ()
main = do
  input <- readFile "inputs/2022/day-11.txt"
  putStrLn $ "Part 1 = " ++ show (part1 input)
  putStrLn $ "Part 2 = " -- ++ show (part1 input)

replace :: Int -> a -> [a] -> [a]
replace pos newVal list = take pos list ++ newVal : drop (pos + 1) list
