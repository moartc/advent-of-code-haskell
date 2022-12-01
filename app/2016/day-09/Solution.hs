hasMarker :: String -> Bool
hasMarker = elem '('

split :: String -> [String]
split "" = []
split string = takeWhile (/= '(') string : processSnd (dropWhile (/= '(') string)

processSnd :: [Char] -> [String]
processSnd "" = []
processSnd string = toAdd : split restToProcess
  where
    fstStr = drop 1 $ takeWhile (/= 'x') string
    fstInt = read fstStr :: Int
    toAppend = takeWhile (/= ')') string
    afterMarker = drop 1 $ dropWhile (/= ')') string
    toAdd = toAppend ++ ")" ++ take fstInt afterMarker
    restToProcess = drop fstInt afterMarker

takeSize :: [Char] -> Int
takeSize string = first * second
  where
    first = read $ takeWhile (/= 'x') formula :: Int
    second = read $ drop 1 $ dropWhile (/= 'x') formula :: Int
    formula = getMarker string
    getMarker = takeWhile (/= ')') . tail . dropWhile (/= '(')

numForPatter :: String -> Int
numForPatter string = if '(' `elem` string then takeSize string else length string

-- PART 2 --
takeSizePart2 :: [Char] -> Int
takeSizePart2 string = second * sum (map numForPatter2 (split rest))
  where
    second = read $ drop 1 $ dropWhile (/= 'x') $ getMarker string :: Int
    rest = drop 1 $ dropWhile (/= ')') string
    getMarker = takeWhile (/= ')') . tail . dropWhile (/= '(')

numForPatter2 :: String -> Int
numForPatter2 string = if '(' `elem` string then takeSizePart2 string else length string

answer :: (String -> Int) -> String -> Int
answer numForPatter = sum . map numForPatter . split . takeWhile (/= '\n')

main :: IO ()
main = do
  input <- readFile "inputs/2016/day-09.txt"
  putStrLn $ "Part 1 = " ++ show (answer numForPatter input)
  putStrLn $ "Part 2 = " ++ show (answer numForPatter2 input)
