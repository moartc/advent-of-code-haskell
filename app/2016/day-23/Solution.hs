import Data.List (isPrefixOf)
import Data.Map ((!))
import qualified Data.Map as M

type Reg = Char

type Registers = M.Map Char Int

data Val = Reg Reg | IntVal Int deriving (Show)

data Instruction = Inc Reg | Dec Reg | Cpy Val Val | Jnz Val Val | Tgl Reg deriving (Show)

parse :: String -> Instruction
parse s
  | "inc " `isPrefixOf` s = parseInc s
  | "dec " `isPrefixOf` s = parseDec s
  | "cpy " `isPrefixOf` s = parseCpy s
  | "jnz " `isPrefixOf` s = parseJnz s
  | "tgl " `isPrefixOf` s = parseTgl s
  | otherwise = error "cannot parse"

parseInc :: String -> Instruction
parseInc s = Inc $ s !! 4

parseDec :: String -> Instruction
parseDec s = Dec $ s !! 4

parseCpy :: String -> Instruction
parseCpy input = Cpy f s
  where
    (first, second) = break (== ' ') $ drop 4 input
    f = if isReg first then Reg $ head first else IntVal (read first :: Int)
    fixedSec = tail second
    s = if isReg fixedSec then Reg $ head fixedSec else IntVal (read fixedSec :: Int)

parseJnz :: String -> Instruction
parseJnz input = Jnz f s
  where
    (first, second) = break (== ' ') $ drop 4 input
    f = if isReg first then Reg $ head first else IntVal (read first :: Int)
    fixedSec = tail second
    s = if isReg fixedSec then Reg $ head fixedSec else IntVal (read fixedSec :: Int)

parseTgl :: String -> Instruction
parseTgl s = Tgl $ s !! 4

isReg :: [Char] -> Bool
isReg x = length x == 1 && (first == 'a' || first == 'b' || first == 'c' || first == 'd')
  where
    first = head x

parseInput :: [String] -> [Instruction]
parseInput = map parse

operation :: Registers -> Int -> [Instruction] -> Instruction -> (Registers, [Instruction])
operation regs _ instr (Inc reg) = (updateReg, instr)
  where
    incVal = regs ! reg + 1
    updateReg = M.insert reg incVal regs
operation regs _ instr (Dec reg) = (updateReg, instr)
  where
    incVal = regs ! reg -1
    updateReg = M.insert reg incVal regs
operation regs _ instr (Cpy _ (IntVal reg)) = (regs, instr)
operation regs _ instr (Cpy (IntVal v) (Reg reg)) = (M.insert reg v regs, instr)
operation regs _ instr (Cpy (Reg reg1) (Reg reg)) = (newReg, instr)
  where
    newVal = regs ! reg1
    newReg = M.insert reg newVal regs
operation regs _ instr (Jnz _ _) = (regs, instr)
operation regs idx allInstructions (Tgl reg) = if length allInstructions <= (idx + regVal) then (regs, allInstructions) else (regs, updatedInstructions)
  where
    instr = allInstructions !! (idx + regVal)
    regVal = regs ! reg
    newInstr = toggle instr
    updatedInstructions = updateAt allInstructions (idx + regVal) newInstr

toggle :: Instruction -> Instruction
toggle (Inc a) = Dec a
toggle (Dec a) = Inc a
toggle (Tgl a) = Inc a
toggle (Jnz a b) = Cpy a b
toggle (Cpy a b) = Jnz a b

updateAt :: [Instruction] -> Int -> Instruction -> [Instruction]
updateAt all idx new = begin ++ [new] ++ end
  where
    begin = take idx all
    end = tail $ drop idx all

play :: [Instruction] -> Registers -> Int -> (Registers, Int)
play instructions registers idx
  | idx >= length instructions = (registers, -1)
  | otherwise = play newInstr updatedReg nextIdx
  where
    (updatedReg, nextIdx, newInstr) = playSingle instructions (instructions !! idx) registers idx

playSingle :: [Instruction] -> Instruction -> Registers -> Int -> (Registers, Int, [Instruction])
playSingle instr (Inc 'a') registers idx = (updatedRegs, newIdx, instr)
  where
    (updatedRegs, newIdx) = tryOptimize1 instr registers idx
playSingle instr (Jnz (IntVal v) (IntVal i)) registers idx
  | v == 0 = (registers, idx + 1, instr)
  | otherwise = (registers, idx + i, instr)
playSingle instr (Jnz (Reg r) (IntVal i)) registers idx
  | registers ! r == 0 = (registers, idx + 1, instr)
  | otherwise = (registers, idx + i, instr)
playSingle instr (Jnz (IntVal v) (Reg r2)) registers idx
  | v == 0 = (registers, idx + 1, instr)
  | otherwise = (registers, idx + jump, instr)
  where
    jump = registers ! r2
playSingle instr (Jnz (Reg r) (Reg r2)) registers idx
  | registers ! r == 0 = (registers, idx + 1, instr)
  | otherwise = (registers, idx + jump, instr)
  where
    jump = registers ! r2
playSingle instr op@(Tgl x) registers idx = (registers, idx + 1, updatedInstructions)
  where
    (regs, updatedInstructions) = operation registers idx instr op
playSingle instr singleInstr registers idx = (updatedRegs, idx + 1, instr)
  where
    (updatedRegs, _) = operation registers idx instr singleInstr

getResult :: String -> Registers -> Int
getResult input regs = fst (play (parseInput $ lines input) regs 0) ! 'a'

tryOptimize1 :: [Instruction] -> Registers -> Int -> (Registers, Int)
tryOptimize1 inst regs idx = ans
  where
    imin1 = inst !! (idx -1)
    i0 = inst !! idx
    i1 = inst !! (idx + 1)
    i2 = inst !! (idx + 2)
    i3 = inst !! (idx + 3)
    i4 = inst !! (idx + 4)
    ans = if opt imin1 i0 i1 i2 i3 i4 then (updatedRegs, idx + 4) else (incRegs, idx + 1)
    valA = (regs ! 'a') + ((regs ! 'c') * (regs ! 'd'))        
    updatedRegs = M.insert 'c' 0 $ M.insert 'd' 0 $ M.insert 'a' valA regs
    (incRegs, _) = operation regs idx inst (inst !! idx)

opt :: Instruction -> Instruction -> Instruction -> Instruction -> Instruction -> Instruction -> Bool
opt (Cpy _ _) (Inc 'a') (Dec 'c') (Jnz (Reg 'c') (IntVal (-2))) (Dec 'd') (Jnz (Reg 'd') (IntVal (-5))) = True
opt _ _ _ _ _ _ = False

main :: IO ()
main = do
  input <- readFile "inputs/2016/day-23.txt"
  putStrLn $ "Part 1 = " ++ show (getResult input $ M.fromList [('a', 7), ('b', 0), ('c', 0), ('d', 0)])
  putStrLn $ "Part 2 = " ++ show (getResult input $ M.fromList [('a', 12), ('b', 0), ('c', 0), ('d', 0)])
