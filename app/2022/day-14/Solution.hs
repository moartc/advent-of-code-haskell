import Data.Char (digitToInt)
import Data.List.Split (splitOn)
import Data.Tree (flatten)
import Data.Set (Set, member, findMax)

inp = "498,4 -> 498,6 -> 496,6\n503,4 -> 502,4 -> 502,9 -> 494,9"
i1 = "498,4 -> 498,6 -> 496,6"

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



singleBall :: [(Int, Int)] -> (Int, Int) -> (Int, Int)
singleBall borders b@(ballX, ballY)
  | next `elem` borders = if nextLeft `notElem` borders then nextLeft else if nextRight `notElem` borders then nextRight else b
  | snd b == maxY = b
  | otherwise = singleBall borders next
    where
      next = (ballX, ballY+1)
      nextLeft = (ballX-1, ballY+1)
      nextRight = (ballX+1, ballY+1)
      maxY = maximum $ map snd borders





main :: IO ()
main = do
  input <- readFile "inputs/2022/day-14.txt"
  putStrLn $ "Part 1 = " ++ show (parseAllLines input)
  putStrLn $ "Part 2 = " -- ++ show (part2 input)
