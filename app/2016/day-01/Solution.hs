split :: String -> [String]
split [] = []
split s = takeWhile (/= ',') s : split (drop 2 $ dropWhile (/= ',') s)

data Move = Move Char Int

strToTurn :: String -> Move
strToTurn s = Move (head s) ((read $ tail s) :: Int)

data Direction = N | S | E | W deriving (Show)

data Position = Position Direction Int Int

instance Eq Position where
  (==) (Position _ x1 y1) (Position _ x2 y2) = x1 == x2 && y1 == y2

newDirection :: Direction -> Char -> Direction
newDirection N d = if d == 'L' then W else E
newDirection S d = if d == 'L' then E else W
newDirection E d = if d == 'L' then N else S
newDirection W d = if d == 'L' then S else N

updateX :: Int -> Direction -> Int -> Int
updateX x dir dist = case dir of
  E -> x + dist
  W -> x - dist
  _ -> x

updateY :: Int -> Direction -> Int -> Int
updateY y dir dist = case dir of
  N -> y + dist
  S -> y - dist
  _ -> y

newPosition :: Position -> Move -> Position
newPosition (Position dir x y) (Move rot dist) = Position nd nX nY
  where
    nd = newDirection dir rot
    nX = updateX x nd dist
    nY = updateY y nd dist

finalPosition :: Position -> [Move] -> Position
finalPosition = foldl newPosition

distance :: Maybe Position -> Int
distance (Just (Position _ x y)) = abs x + abs y
distance Nothing = 0

part1 :: String -> Int
part1 input = distance $ Just . finalPosition (Position N 0 0) . map strToTurn $ split input

allVisited :: Position -> [Move] -> [Position]
allVisited p [] = [p]
allVisited p (x : xs) = newP : allVisited newP xs where newP = newPosition p x

allVisitedPart2 :: Position -> [Move] -> [Position]
allVisitedPart2 p [] = [p]
allVisitedPart2 p [x] = allVisitedForMove p x
allVisitedPart2 p (x : xs) =
  oneMovePositions ++ allVisitedPart2 (last oneMovePositions) xs
  where
    oneMovePositions = allVisitedForMove p x

allVisitedForMove :: Position -> Move -> [Position]
allVisitedForMove (Position dir x y) (Move rot dist) =
  tail
    [ Position nd nX nY | nd <- [newDirection dir rot], let updatedX = updateX x nd dist, let updatedY = updateY y nd dist, nX <- if updatedX > x then [x .. updatedX] else reverse [updatedX .. x], nY <- if updatedY > y then [y .. updatedY] else reverse [updatedY .. y]
    ]

firstDup :: Eq a => [a] -> Maybe a
firstDup [] = Nothing
firstDup (x : xs) = if x `elem` xs then Just x else firstDup xs

part2 :: String -> Int
part2 input = distance . firstDup $ allVisitedPart2 (Position N 0 0) (map strToTurn $ split input)

main :: IO ()
main = do
  input <- readFile "inputs/2016/day-01.txt"
  putStrLn $ "Part 1 = " ++ show (part1 input)
  putStrLn $ "Part 2 = " ++ show (part2 input)
