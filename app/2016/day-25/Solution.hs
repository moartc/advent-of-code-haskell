import Data.Char (intToDigit)
import Data.List (isPrefixOf)
import Data.Map ((!))
import qualified Data.Map as M

type Reg = Char

type Registers = M.Map Char Int

data Val = Reg Reg | IntVal Int deriving (Show)

data Instruction = Inc Reg | Dec Reg | Cpy Val Reg | Jnz Val Int | Out Reg deriving (Show)

parse :: String -> Instruction
parse s
  | "inc " `isPrefixOf` s = parseInc s
  | "dec " `isPrefixOf` s = parseDec s
  | "cpy " `isPrefixOf` s = parseCpy s
  | "jnz " `isPrefixOf` s = parseJnz s
  | "out " `isPrefixOf` s = parseOut s
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

parseOut :: String -> Instruction
parseOut s = Out $ s !! 4

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
operation regs (Out _) = regs

play :: String -> [Instruction] -> Registers -> Int -> (Registers, Int, Bool)
play out instructions registers idx
  | idx >= length instructions = (registers, -1, False)
  | length out > 14 && not ("0101010101" `isPrefixOf` out) = (registers, -1, False)
  | "0101010101" `isPrefixOf` out = (registers, -1, True)
  | otherwise = play lastOut instructions updatedReg nextIdx
  where
    (updatedReg, nextIdx, lastOut) = playSingle out (instructions !! idx) registers idx

playSingle :: String -> Instruction -> Registers -> Int -> (Registers, Int, String)
playSingle out (Jnz (IntVal v) i) registers idx
  | v == 0 = (registers, idx + 1, out)
  | otherwise = (registers, idx + i, out)
playSingle out (Jnz (Reg r) i) registers idx
  | registers ! r == 0 = (registers, idx + 1, out)
  | otherwise = (registers, idx + i, out)
playSingle out (Out reg) registers idx = (registers, idx + 1, newOut)
  where
    regV = registers ! reg
    newOut = out ++ [intToDigit regV]
playSingle out i registers idx = (operation registers i, idx + 1, out)

getResult :: String -> Int
getResult input = idxToRet
  where
    regs x = M.fromList [('a', x), ('b', 0), ('c', 0), ('d', 0)]
    inp = parseInput $ lines input
    (_, idxToRet) = head $ filter (\((_, _, result), index) -> result) [(res, r) | r <- [0 ..], let res = play "" inp (regs r) 0]

main :: IO ()
main = do
  input <- readFile "inputs/2016/day-25.txt"
  putStrLn $ "The only part = " ++ show (getResult input)
