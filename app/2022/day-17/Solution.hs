import Debug.Trace
import GHC.Base (Any)

get :: [a] -> Int -> a
get list index = list !! safeIdx list index

safeIdx :: Foldable t => t a -> Int -> Int
safeIdx list index
  | index < 0 = (index `mod` size + size) `mod` size
  | otherwise = index `mod` size
  where
    size = length list

type ListPair = [(Int, Int)]

getStartPos :: ListPair -> Int -> ListPair
getStartPos rock highestY = map (\x -> (fst x + highestY + 4, snd x + 2)) rock

moveLeft :: ListPair -> ListPair -> ListPair
moveLeft chamber rock
  | canMoveLeft chamber rock = map (\x -> (fst x, snd x -1)) rock
  | otherwise = rock

moveRight :: ListPair -> ListPair -> ListPair
moveRight chamber rock
  | canMoveRight chamber rock = map (\x -> (fst x, snd x + 1)) rock
  | otherwise = rock

moveDown :: ListPair -> ListPair
moveDown = map (\x -> (fst x -1, snd x))

canMoveLeft :: ListPair -> ListPair -> Bool
canMoveLeft chamber rock
  | minimum (map snd rock) <= 0 = False
  | otherwise = not (any (`elem` rockOnLeft) chamber)
  where
    rockOnLeft = map (\p -> (fst p, snd p -1)) rock

canMoveRight :: ListPair -> ListPair -> Bool
canMoveRight chamber rock
  | maximum (map snd rock) >= 6 = False
  | otherwise = not (any (`elem` rockOnRight) chamber)
  where
    rockOnRight = map (\p -> (fst p, snd p + 1)) rock

shouldStop :: ListPair -> ListPair -> Bool
shouldStop chamber rock
  | fst (head rock) == 0 = True
  | otherwise = any (\x -> (fst x -1, snd x) `elem` chamber) rock

part1 :: String -> [(Int, Int)] -> [[(Int, Int)]] -> Int -> Int -> Int -> Int
part1 moves chamber rocks repeatNum rockIndex moveIdx
  | rockIndex == repeatNum = maximum (map fst chamber) + 1
  | otherwise = part1 moves newChamber rocks repeatNum (rockIndex + 1) newMoveIdx
  where
    rock = get rocks rockIndex
    rockToMoveFirst = getStartPos rock (if null chamber then -1 else maximum $ map fst chamber)
    rockToMove = if get moves moveIdx == '<' then moveLeft chamber rockToMoveFirst else moveRight chamber rockToMoveFirst
    (whileRes, newMoveIdx) = whileLoop rockToMove chamber moves $ moveIdx + 1
    newChamber = chamber ++ whileRes

whileLoop :: ListPair -> ListPair -> [Char] -> Int -> (ListPair, Int)
whileLoop rockToMove chamber moves moveIdx
  | shouldStop chamber rockToMove = (rockToMove, moveIdx)
  | otherwise = whileLoop moved chamber moves (moveIdx + 1)
  where
    movedDown = moveDown rockToMove
    moved = if get moves moveIdx == '<' then moveLeft chamber movedDown else moveRight chamber movedDown

part1Play :: String -> Int
part1Play inp = part1 inp [] rocks 2022 0 0
  where
    rock1 = [(0, 0), (0, 1), (0, 2), (0, 3)]
    rock2 = [(0, 1), (1, 0), (1, 1), (1, 2), (2, 1)]
    rock3 = [(0, 0), (0, 1), (0, 2), (1, 2), (2, 2)]
    rock4 = [(0, 0), (1, 0), (2, 0), (3, 0)]
    rock5 = [(0, 0), (0, 1), (1, 0), (1, 1)]
    rocks = [rock1, rock2, rock3, rock4, rock5]

------------------------------------------ PART 2 ------------------------------------------

part2 :: [Char] -> [(Int, Int)] -> [ListPair] -> Int -> Int -> Int -> Int -> Int -> [Int] -> [Int]
part2 moves chamber rocks repeatNum rockIndex moveIdx prev current diffs
  | rockIndex == repeatNum = newDiffs
  | otherwise = part2 moves newChamber rocks repeatNum (rockIndex + 1) newMoveIdx current newCurrent newDiffs
  where
    rock = get rocks rockIndex
    rockToMoveFirst = getStartPos rock (if null chamber then -1 else maximum $ map fst chamber)
    rockToMove = if get moves moveIdx == '<' then moveLeft chamber rockToMoveFirst else moveRight chamber rockToMoveFirst
    (whileRes, newMoveIdx) = whileLoop2 rockToMove chamber moves $ moveIdx + 1
    newChamber = chamber ++ whileRes
    newCurrent = maximum (map fst newChamber) + 1
    newDiffs = diffs ++ [newCurrent - current]

whileLoop2 :: ListPair -> ListPair -> [Char] -> Int -> (ListPair, Int)
whileLoop2 rockToMove chamber moves moveIdx
  | shouldStop chamber rockToMove = (rockToMove, moveIdx + 1)
  | otherwise = whileLoop moved chamber moves (moveIdx + 1)
  where
    movedDown = moveDown rockToMove
    moved = if get moves moveIdx == '<' then moveLeft chamber movedDown else moveRight chamber movedDown

sublist :: Int -> Int -> [a] -> [a]
sublist x y = drop x . take y

findPeriodAndPrevResult :: [Int] -> ([Int], Int)
findPeriodAndPrevResult diffs =
  head $
    [ (newSub1, i)
      | i <- [0 .. 5000 - 50 -1],
        let sub1 = sublist i (i + 50) diffs,
        j <- [i + 50 .. 5000 - 50 -1],
        let sub2 = sublist j (j + 50) diffs,
        sub1 == sub2,
        let newSub1 = sublist i j diffs,
        let newSub2 = sublist j (j + length newSub1) diffs
    ]

part2Play :: String -> Int
part2Play inp = sum (take firstIndex diffs) + (numGr * sum period) + sum (take rest period)
  where
    rock1 = [(0, 0), (0, 1), (0, 2), (0, 3)]
    rock2 = [(0, 1), (1, 0), (1, 1), (1, 2), (2, 1)]
    rock3 = [(0, 0), (0, 1), (0, 2), (1, 2), (2, 2)]
    rock4 = [(0, 0), (1, 0), (2, 0), (3, 0)]
    rock5 = [(0, 0), (0, 1), (1, 0), (1, 1)]
    rocks = [rock1, rock2, rock3, rock4, rock5]
    diffs = part2 inp [] rocks 5000 0 0 0 0 []
    (period, firstIndex) = findPeriodAndPrevResult diffs
    toCalc = 1000000000000 - firstIndex
    numGr = toCalc `div` length period
    rest = toCalc `mod` length period

main :: IO ()
main = do
  input <- readFile "inputs/2022/day-17.txt"
  putStrLn $ "Part 1 = " ++ show (part1Play input)
  putStrLn $ "Part 2 = " ++ show (part2Play input)
