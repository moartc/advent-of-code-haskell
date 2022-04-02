import qualified Data.Maybe
data Coord = Coord Int Int deriving (Show)

type Button = (Char, Coord)

keypadPart1 :: [Button]
keypadPart1 =
  [ ('1', Coord 0 0),
    ('2', Coord 1 0),
    ('3', Coord 2 0),
    ('4', Coord 0 1),
    ('5', Coord 1 1),
    ('6', Coord 2 1),
    ('7', Coord 0 2),
    ('8', Coord 1 2),
    ('9', Coord 2 2)
  ]

firstFromTuple :: Coord -> [Button] -> Maybe Button
firstFromTuple (Coord _ y) [] = Nothing
firstFromTuple c@(Coord x y) (b@(_, Coord x2 y2) : xs) =
  if x == x2 && y == y2
    then Just b
    else firstFromTuple c xs

getNextCoord :: Coord -> Char -> Coord
getNextCoord (Coord x y) m = case m of
  'U' -> Coord x $ y - 1
  'D' -> Coord x $ y + 1
  'L' -> Coord (x - 1) y
  'R' -> Coord (x + 1) y
  _ -> Coord x y

getNextButton :: Maybe Button -> Char -> [Button] -> Maybe Button
getNextButton (Just (_, c)) m keypad = firstFromTuple (getNextCoord c m) keypad
getNextButton Nothing _ _ = Nothing

collectButtonsForGroup :: Maybe Button -> String -> [Button] -> [Maybe Button]
collectButtonsForGroup _ [] _ = []
collectButtonsForGroup button (x : xs) keypad =
  if Data.Maybe.isJust nextButton
    then nextButton : collectButtonsForGroup nextButton xs keypad
    else Nothing : collectButtonsForGroup button xs keypad
  where
    nextButton = getNextButton button x keypad

collectButtonsForAll :: Maybe Button -> [String] -> [Button] -> [Button]
collectButtonsForAll _ [] _ = []
collectButtonsForAll button (x : xs) keypad = lastInGroup : collectButtonsForAll (Just lastInGroup) xs keypad
  where
    lastInGroup = last [x | Just x <- collectButtonsForGroup button x keypad]

part1 :: [String] -> [Char]
part1 input = [x | (x, _) <- collectButtonsForAll (Just ('5', Coord 1 1)) input keypadPart1]

-- PART 2 --
keypadPart2 :: [Button]
keypadPart2 =
  [ ('1', Coord 2 0),
    ('2', Coord 1 1),
    ('3', Coord 2 1),
    ('4', Coord 3 1),
    ('5', Coord 0 2),
    ('6', Coord 1 2),
    ('7', Coord 2 2),
    ('8', Coord 3 2),
    ('9', Coord 4 2),
    ('A', Coord 1 3),
    ('B', Coord 2 3),
    ('C', Coord 3 3),
    ('D', Coord 2 4)
  ]

part2 :: [String] -> [Char]
part2 input = [x | (x, _) <- collectButtonsForAll (Just ('5', Coord 0 2)) input keypadPart2]

main :: IO ()
main = do
  input <- readFile "inputs/day-02.txt"
  putStrLn $ "Part 1 = " ++ show (part1 (lines input))
  putStrLn $ "Part 2 = " ++ show (part2 (lines input))