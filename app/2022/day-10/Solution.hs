import Data.List.Split (splitOn)

type State = (Int, Int, [Int])

data Inst = Noop | Add Int

parseInp :: String -> [Inst]
parseInp inp = [if inst == "noop" then Noop else Add (read (last $ splitOn " " inst) :: Int) | inst <- lines inp]

part1 :: String -> Int
part1 input = sum finalList
  where
    (_, _, finalList) = foldl process (1, 0, []) $ parseInp input

process :: State -> Inst -> State
process (x, cycle, list) Noop = (x, cycle + 1, fstUpdateList)
  where
    fstUpdateList = updateSignals list (cycle + 1) x
process (x, cycle, list) (Add val) = (x + val, cycle + 2, sndUpdate)
  where
    fstUpdate = updateSignals list (cycle + 1) x
    sndUpdate = updateSignals fstUpdate (cycle + 2) x

updateSignals :: [Int] -> Int -> Int -> [Int]
updateSignals list cycle x = if (cycle - 20) `mod` 40 == 0 then list ++ [x * (cycle)] else list

-- PART 2

type State2 = (String, Int, Int, Int)

buildOutputAndUpdateCrt :: State2 -> State2
buildOutputAndUpdateCrt (output, crtPos, x, cycle) = (output ++ toAdd, newCrtPos, x, cycle)
  where
    char = if crtPos - x >= -1 && crtPos - x <= 1 then '#' else '.'
    toAdd = if cycle `mod` 40 == 0 then char : ['\n'] else [char]
    newCrtPos = (crtPos + 1) `mod` 40

process2 :: State2 -> Inst -> State2
process2 (output, crtPos, x, cycle) Noop = buildOutputAndUpdateCrt (output, crtPos, x, cycle + 1)
process2 (output, crtPos, x, cycle) (Add v) = (output3, crtPos3, x + v, cycle3)
  where
    (output2, crtPos2, _, cycle2) = buildOutputAndUpdateCrt (output, crtPos, x, cycle + 1)
    (output3, crtPos3, _, cycle3) = buildOutputAndUpdateCrt (output2, crtPos2, x, cycle2 + 1)

part2 :: String -> String
part2 input = finalOutput
  where
    (finalOutput, _, _, _) = foldl process2 ("", 0, 1, 0) $ parseInp input

main :: IO ()
main = do
  input <- readFile "inputs/2022/day-10.txt"
  putStrLn $ "Part 1 = " ++ show (part1 input)
  putStrLn "Part 2:"
  mapM_ print (lines $ part2 input)
