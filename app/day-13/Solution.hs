import Control.Monad (when)
import Data.Char (digitToInt)
import qualified Data.Map as M
import Data.Set ( insert, member, Set )
import qualified Data.Set as S hiding (foldl)
import GHC.Show ( intToDigit )

formula :: Int -> Int -> Int -> Int
formula y x officeNumber = ((x * x) + (3 * x) + (2 * x * y) + y + (y * y)) + officeNumber

intToBin :: Int -> String
intToBin 1 = ['1']
intToBin 0 = ['0']
intToBin int = intToBin (int `div` 2) ++ [(intToDigit $ int `mod` 2) :: Char]

isOpenStr :: String -> Bool
isOpenStr = even . length . filter (== '1')

isOpen :: Int -> Int -> Int -> Bool
isOpen y x officeNumber = isOpenStr (intToBin (formula y x officeNumber))

getCoords :: Int -> Int -> Int -> Int -> [(Int, Int)]
getCoords y x yLim xLim = filtered
  where
    list = [(y, x + 1), (y -1, x), (y + 1, x), (y, x -1)]
    filtered = filter (\p -> isInRange p yLim xLim) list

isInRange :: (Int, Int) -> Int -> Int -> Bool
isInRange coord yLim xLim = y >= 0 && y < yLim && x >= 0 && x < xLim
  where
    y = fst coord
    x = snd coord

playGame :: Int -> Int -> Int -> Set (Int, Int) -> Int -> Int -> Int -> Int -> Int
playGame y x counter visited searchedY searchedX limit officeNumber
  | y == searchedY && x == searchedX = counter
  | otherwise = minimum ([uncurry playGame c (counter + 1) updatedVisited searchedY searchedX limit officeNumber | c <- getCoords y x limit limit, uncurry isOpen c officeNumber && not (member c visited)] ++ [9999])
  where
    updatedVisited = insert (y, x) visited

part1 :: Int -> Int -> Int -> Int
part1 y x = playGame 1 1 0 S.empty y x 100

playGamePart2 :: Int -> Int -> Int -> Int -> Int -> M.Map (Int, Int) Int -> M.Map (Int, Int) Int
playGamePart2 y x counter limit officeNumber visited
  | counter > 50 = visited
  | otherwise = foldl (\map c -> if uncurry isOpen c officeNumber && shouldVisit c map counter then uncurry playGamePart2 c (counter + 1) limit officeNumber map else map) updatedVisited $ getCoords y x limit limit
  where
    shouldVisit c map counter = not (M.member c map) || (map M.! c > counter)
    updatedVisited = if shouldVisit (y, x) visited counter then M.insert (y, x) counter visited else visited

part2 :: Int -> Int
part2 officeNumber = length $ playGamePart2 1 1 0 200 officeNumber M.empty

main :: IO ()
main = do
  putStrLn $ "Part 1 = " ++ show (part1 39 31 1362)
  putStrLn $ "Part 2 = " ++ show (part2 1362)
