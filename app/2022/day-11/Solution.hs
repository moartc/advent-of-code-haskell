import Data.Char (digitToInt, isDigit)
import Data.List (sort)
import Data.List.Split (splitOn)

data Monkey = Monkey {items :: [Int], op :: Char, opRightSide :: Either Int String, divisibleBy :: Int, ifTrue :: Int, ifFalse :: Int} deriving (Show)

addItem :: Monkey -> Int -> Monkey
addItem m i = m {items = items m ++ [i]}

resetItems :: Monkey -> Monkey
resetItems m = m {items = []}

newVal :: Int -> Char -> Either Int String -> Int
newVal int char (Left newVal) = if char == '*' then int * newVal else int + newVal
newVal int char (Right _) = if char == '*' then int * int else int + int

type RoundState = ([Int], [Monkey])

oneMonkeyRound :: RoundState -> Int -> Bool -> RoundState
oneMonkeyRound (insp, ms) monkeyIdx divBy3 = (updatedInsp, fst newState)
  where
    newState = monkeyRound ms monkeyIdx divBy3
    updatedInsp = replace monkeyIdx ((insp !! monkeyIdx) + snd newState) insp

part1 :: String -> Int
part1 inp = product $ take 2 $ reverse $ sort $ fst finalRS
  where
    monkeys = parseInput inp
    initRoundState = (replicate (length monkeys) 0, monkeys)
    finalRS = foldl (\x y -> allMonkeysRound x 0 True) initRoundState [1 .. 20]

part2 :: String -> Int
part2 inp = product $ take 2 $ reverse $ sort $ fst finalRS
  where
    monkeys = parseInput inp
    initRoundState = (replicate (length monkeys) 0, monkeys)
    finalRS = foldl (\x y -> allMonkeysRound x 0 False) initRoundState [1 .. 10000]

allMonkeysRound :: RoundState -> Int -> Bool -> RoundState
allMonkeysRound rs idx divBy3 = foldl (\r y -> oneMonkeyRound r y divBy3) rs [0 .. length (snd rs) -1]

monkeyRound :: [Monkey] -> Int -> Bool -> ([Monkey], Int)
monkeyRound monkeys idx divBy3 = (replace idx resetMonk firstUpdate, length $ items monk)
  where
    monk = monkeys !! idx
    firstUpdate = updateMonkeys monkeys [handleItem monk item allDivs divBy3 | item <- items monk]
    resetMonk = resetItems monk
    allDivs = product $ map divisibleBy monkeys

updateMonkeys :: [Monkey] -> [(Int, Int)] -> [Monkey]
updateMonkeys all ((toReplace, newValue) : us) = updateMonkeys replaced us
  where
    monkeyToReplace = all !! toReplace
    updated = addItem monkeyToReplace newValue
    replaced = replace toReplace updated all
updateMonkeys all [] = all

handleItem :: Monkey -> Int -> Int -> Bool -> (Int, Int)
handleItem monkey itemVal allDivs divBy3 = (throwTo, valToThrow)
  where
    newValue = newVal itemVal (op monkey) (opRightSide monkey)
    valToThrow = newValue `mod` allDivs `div` (if divBy3 then 3 else 1)
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

data Expression
  = Lit Float
  | Add Expression Expression
  | Mul Expression Expression
  | Sub Expression Expression
  | Div Expression Expression deriving(Show)

x = Add (Lit 1.2) (Lit 1.2)

main :: IO ()
main = do
  input <- readFile "inputs/2022/day-11.txt"
  putStrLn $ "Part 1 = " ++ show (part1 input)
  putStrLn $ "Part 2 = " ++ show (part2 input)

replace :: Int -> a -> [a] -> [a]
replace pos newVal list = take pos list ++ newVal : drop (pos + 1) list
