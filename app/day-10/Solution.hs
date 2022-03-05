import Data.Char (isDigit)
import Data.Either (isLeft, isRight, lefts, rights)
import Data.List (isPrefixOf)
import Control.Arrow (ArrowChoice(left))

main :: IO ()
main = do
  input <- readFile "inputs/day-10.txt"
  putStrLn "Part 1 = "

inp = "value 5 goes to bot 2\nbot 2 gives low to bot 1 and high to bot 0\nvalue 3 goes to bot 1\nbot 1 gives low to output 1 and high to bot 0\nbot 0 gives low to output 2 and high to output 0\nvalue 2 goes to bot 2"

type BotInput = (Int, String, String)

type ValueInput = (Int, Int)

parseInput :: [String] -> [Either ValueInput BotInput]
parseInput = map parseString

parseString :: String -> Either ValueInput BotInput
parseString s = if "value" `isPrefixOf` s then Left $ parseValueInput s else Right $ parseBotInput s

parseValueInput :: String -> ValueInput
parseValueInput string = (read val :: Int, read $ reverse bot)
  where
    dropped = dropWhile (not . isDigit) string
    val = takeWhile (/= ' ') dropped
    bot = takeWhile (/= ' ') (reverse dropped)

parseBotInput :: String -> BotInput
parseBotInput string = (read bot, fst, snd)
  where
    dropped = takeWhile (/= ' ') (drop 4 string)
    bot = takeWhile (/= ' ') dropped
    fst = init $ takeWhile (/= 'a') $ drop 14 $ dropWhile isDigit (drop 4 string)
    snd = drop 8 $ dropWhile (/= 'h') string

parsedInput = parseInput $ lines inp

{-
b0:
b1: v3
b2: v5 v2

out0
out1
out2
-}