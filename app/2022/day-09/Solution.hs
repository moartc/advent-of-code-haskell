import Data.Char (digitToInt)
import Data.List.Split (splitOn)
import Data.Set (Set, empty, fromList, insert)

data Pos = Pos {y :: Int, x :: Int} deriving (Show, Eq, Ord)

solve :: String -> Int -> Int
solve inp idx = length collected
  where
    parsed = parse inp
    rope = replicate (idx + 1) $ Pos 0 0
    collected = collect idx parsed rope $ fromList [Pos 0 0]

parse :: String -> String
parse inp = concat [replicate (read (arr !! 1) :: Int) (head $ head arr) | arr <- map (splitOn " ") $ lines inp]

collect :: Int -> String -> [Pos] -> Set Pos -> Set Pos
collect idx (m : mx) pos set = collect idx mx newPos newSet
  where
    newPos = doMove m pos
    newPosIdx = newPos !! idx
    newSet = insert newPosIdx set
collect _ [] _ set = set

doMove :: Char -> [Pos] -> [Pos]
doMove dir current = updated
  where
    updatedHead = newHead dir $ head current
    updated = updateTail [updatedHead] $ tail current

updateTail :: [Pos] -> [Pos] -> [Pos]
updateTail updates (x : xs) = updateTail newUpdate xs
  where
    updated = updateSingle (last updates) x
    newUpdate = updates ++ [updated]
updateTail updates [] = updates

updateSingle :: Pos -> Pos -> Pos
updateSingle prev curr =
  if abs yDist == 2 || abs xDist == 2
    then
      if abs yDist == 2 && abs xDist == 2
        then
          if yDist > 0 && xDist > 0
            then prev {y = y prev -1, x = x prev -1}
            else
              if yDist < 0 && xDist > 0
                then prev {y = y prev + 1, x = x prev -1}
                else
                  if yDist > 0 && xDist < 0
                    then prev {y = y prev -1, x = x prev + 1}
                    else prev {y = y prev + 1, x = x prev + 1}
        else
          if abs yDist == 2
            then
              if yDist > 0
                then prev {y = y prev -1}
                else prev {y = y prev + 1}
            else
              if xDist > 0
                then prev {x = x prev -1}
                else prev {x = x prev + 1}
    else curr
  where
    yDist = y prev - y curr
    xDist = x prev - x curr

newHead :: Char -> Pos -> Pos
newHead dir pos
  | dir == 'R' = pos {x = x pos + 1}
  | dir == 'L' = pos {x = x pos -1}
  | dir == 'U' = pos {y = y pos + 1}
  | otherwise = pos {y = y pos -1}

main :: IO ()
main = do
  input <- readFile "inputs/2022/day-09.txt"
  putStrLn $ "Part 1 = " ++ show (solve input 1)
  putStrLn $ "Part 2 = " ++ show (solve input 9)
