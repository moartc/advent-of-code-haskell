main :: IO ()
main = do
  input <- readFile "inputs/day-09.txt"
  putStrLn "Part 1 = "

s1 = "A(1x5)BC"

s2 = "ADVENT"

hasMarker :: String -> Bool
hasMarker = elem '('

f1 = takeWhile (/= '(')

f2 = takeWhile (/=')') . tail . dropWhile (/= '(') 