import Data.List (dropWhileEnd)
import Data.List.Split (splitOn)
import Data.Set (fromList)
import Debug.Trace

debug = flip trace

type Inp = ((Int, Int), (Int, Int))

parseInp :: String -> [Inp]
parseInp str = map parseLine $ lines str
  where
    parseLine :: String -> Inp
    parseLine str = ((x1, x2), (x3, x4))
      where
        q = splitOn " " str
        x1 = read $ takeWhile (/= ',') $ drop 2 $ q !! 2 :: Int
        x2 = read $ takeWhile (/= ':') $ drop 2 $ q !! 3 :: Int
        x3 = read $ takeWhile (/= ',') $ drop 2 $ q !! 8 :: Int
        x4 = read $ drop 2 $ q !! 9 :: Int

distance :: (Int, Int) -> (Int, Int) -> Int
distance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

part1 :: [Inp] -> Int -> Int
part1 input pointToCheck = res - sensorAndBeacon
  where
    min = minimum $ map (fst . fst) input ++ map (fst . snd) input
    max = maximum $ map (fst . fst) input ++ map (fst . snd) input
    maxDist = maximum $ map (uncurry distance) input
    res = length $ fromList $ concatMap (\pair -> filter (\x -> distance (fst pair) (x, pointToCheck) <= uncurry distance pair) [min - maxDist .. max + maxDist]) input
    sensorAndBeacon = length $ fromList $ map snd $ filter (\x -> snd (fst x) == pointToCheck || snd (snd x) == pointToCheck) input

main :: IO ()
main = do
  input <- readFile "inputs/2022/day-15.txt"
  putStrLn $ "Part 1 = " ++ show (part1 (parseInp input) 2000000)
  putStrLn $ "Part 2 = " -- ++ show (part2 input)
