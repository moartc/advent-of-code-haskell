import Data.Char (digitToInt)
import Data.List.Split (splitOn)
import Data.Set (Set, delete, findMax, fromList, insert, member)

parseAllLines :: String -> [(Int, Int)]
parseAllLines inp = concat $ concatMap (map (uncurry createLine) . pairs . map ((\p -> (read (head p) :: Int, read (p !! 1) :: Int)) . splitOn ",") . splitOn " -> ") $ lines inp
  where
    pairs (x : y : ys) = (x, y) : pairs (y : ys)
    pairs [x] = []
    pairs [] = []
    createLine (y1, x1) (y2, x2)
      | y1 == y2 = map (\x -> (y1, x)) xs
      | otherwise = map (\y -> (y, x1)) ys
      where
        xs = if x1 > x2 then [x2 .. x1] else [x1 .. x2]
        ys = if y1 > y2 then [y2 .. y1] else [y1 .. y2]

type SingleBall = Set (Int, Int) -> (Int, Int) -> Int -> (Int, Int)

singleBall :: SingleBall
singleBall borders b@(ballX, ballY) maxY
  | next `elem` borders =
    if nextLeft `notElem` borders
      then singleBall borders nextLeft maxY
      else
        if nextRight `notElem` borders
          then singleBall borders nextRight maxY
          else b
  | snd b == maxY = b
  | otherwise = singleBall borders next maxY
  where
    next = (ballX, ballY + 1)
    nextLeft = (ballX -1, ballY + 1)
    nextRight = (ballX + 1, ballY + 1)

singleBallP2 :: SingleBall
singleBallP2 borders b@(ballX, ballY) maxY
  | next `elem` borders || (snd next == maxY) =
    if nextLeft `notElem` borders && (snd nextLeft /= maxY)
      then singleBallP2 borders nextLeft maxY
      else
        if nextRight `notElem` borders && (snd nextRight /= maxY)
          then singleBallP2 borders nextRight maxY
          else b
  | otherwise = singleBallP2 borders next maxY
  where
    next = (ballX, ballY + 1)
    nextLeft = (ballX -1, ballY + 1)
    nextRight = (ballX + 1, ballY + 1)

sim :: ((Int, Int) -> Bool) -> Set (Int, Int) -> Int -> Int -> SingleBall -> Int
sim endPredicate borders counter maxY singleBall = if endPredicate lastResult then counter else sim endPredicate updatedBorders2 (counter + 1) maxY singleBall
  where
    lastResult = singleBall borders (500, 0) maxY
    updatedBorders = insert lastResult borders
    updatedBorders2 = delete oneUnder updatedBorders
    oneUnder = (fst lastResult, snd lastResult + 2)

part1 :: String -> Int
part1 inp = sim (\lastRes -> snd lastRes == maxY) (fromList borders) 0 maxY singleBall
  where
    borders = parseAllLines inp
    maxY = maximum $ map snd borders

part2 :: String -> Int
part2 inp = sim (\lastRes -> lastRes == (500, 0)) (fromList borders) 1 (originalMaxY + 2) singleBallP2
  where
    borders = parseAllLines inp
    originalMaxY = maximum $ map snd borders

main :: IO ()
main = do
  input <- readFile "inputs/2022/day-14.txt"
  putStrLn $ "Part 1 = " ++ show (part1 input)
  putStrLn $ "Part 2 = " ++ show (part2 input)
