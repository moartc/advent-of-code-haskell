import Data.List ( isPrefixOf )
import Data.Map ((!))
import qualified Data.Map as M

type Reg = Char

type Registers = M.Map Char Int

data Val = Reg Reg | IntVal Int deriving (Show)

data Instruction = Inc Reg | Dec Reg | Cpy Val Reg | Jnz Val Int deriving (Show)

parse :: String -> Instruction
parse s
  | "inc " `isPrefixOf` s = parseInc s
  | "dec " `isPrefixOf` s = parseDec s
  | "cpy " `isPrefixOf` s = parseCpy s
  | "jnz " `isPrefixOf` s = parseJnz s
  | otherwise = error "cannot parse"

parseInc :: String -> Instruction
parseInc s = Inc $ s !! 4

parseDec :: String -> Instruction
parseDec s = Dec $ s !! 4

parseCpy :: String -> Instruction
parseCpy input = Cpy f (head $ tail second)
  where
    (first, second) = break (== ' ') $ drop 4 input
    f = if isReg first then Reg $ head first else IntVal (read first :: Int)

parseJnz :: String -> Instruction
parseJnz input = Jnz f (read (tail second) :: Int)
  where
    (first, second) = break (== ' ') $ drop 4 input
    f = if isReg first then Reg $ head first else IntVal (read first :: Int)

isReg :: [Char] -> Bool
isReg x = length x == 1 && (first == 'a' || first == 'b' || first == 'c' || first == 'd')
  where
    first = head x

parseInput :: [String] -> [Instruction]
parseInput = map parse

operation :: Registers -> Instruction -> Registers
operation regs (Inc reg) = updateReg
  where
    incVal = regs ! reg + 1
    updateReg = M.insert reg incVal regs
operation regs (Dec reg) = updateReg
  where
    incVal = regs ! reg -1
    updateReg = M.insert reg incVal regs
operation regs (Cpy (IntVal v) reg) = M.insert reg v regs
operation regs (Cpy (Reg reg1) reg) = newReg
  where
    newVal = regs ! reg1
    newReg = M.insert reg newVal regs
operation regs (Jnz _ _) = regs

play :: [Instruction] -> Registers -> Int -> (Registers, Int)
play instructions registers idx
  | idx >= length instructions = (registers, -1)
  | otherwise = play instructions updatedReg nextIdx
  where
    (updatedReg, nextIdx) = playSingle (instructions !! idx) registers idx

playSingle :: Instruction -> Registers -> Int -> (Registers, Int)
playSingle (Jnz (IntVal v) i) registers idx
  | v == 0 = (registers, idx + 1)
  | otherwise = (registers, idx + i)
playSingle (Jnz (Reg r) i) registers idx
  | registers ! r == 0 = (registers, idx + 1)
  | otherwise = (registers, idx + i)
playSingle i registers idx = (operation registers i, idx + 1)

getResult :: String -> Registers -> Int
getResult input regs = fst gameResult ! 'a'
  where
    gameResult = play (parseInput $ lines input) regs 0

main :: IO ()
main = do
  input <- readFile "inputs/day-12.txt"
  putStrLn $ "Part 1 = " ++ show (getResult input $ M.fromList [('a', 0), ('b', 0), ('c', 0), ('d', 0)])
  putStrLn $ "Part 2 = " ++ show (getResult input $ M.fromList [('a', 0), ('b', 0), ('c', 1), ('d', 0)])
