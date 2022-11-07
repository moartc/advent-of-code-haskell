data Disk = Disk {totalPos :: Int, startPos :: Int} deriving (Show)

parseInput :: String -> [Disk]
parseInput = map parseLine . lines

parseLine :: String -> Disk
parseLine line = Disk allPos currentPos
  where
    allPos = read (takeWhile (/= ' ') $ drop 12 line) :: Int
    currentPos = read (init $ reverse $ takeWhile (/= ' ') $ reverse line) :: Int

getAllNewPos :: [Disk] -> Int -> [Int]
getAllNewPos disks time = [posForTime d time i | (d, i) <- zip disks [1..]]      

solve :: [Disk] -> Int
solve disks = snd $ head $ filter (\(d, t) -> all (== 0) d) [x | t <- [0 ..], let x = (getAllNewPos disks t, t)]  

posForTime :: Disk -> Int -> Int -> Int
posForTime disk time idx = (startPos disk + time + idx) `mod` totalPos disk  

part1 :: String -> Int
part1 = solve . parseInput 

part2 :: String -> Int
part2 input = solve $ parseInput input ++[Disk 11 0]

main :: IO ()
main = do
  input <- readFile "inputs/day-15.txt"  
  putStrLn $ "Part 1 = "  ++ show (part1 input)
  putStrLn $ "Part 2 = "  ++ show (part2 input)
