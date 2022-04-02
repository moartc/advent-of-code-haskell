import Data.Char (isDigit)
import Data.Either (lefts, rights)
import Data.List (isPrefixOf)

type BotInput = (Int, String, String)

type ValueInput = (Int, Int)

data Bot = Bot {number :: Int, low :: Int, high :: Int} deriving (Show)

newBot :: Int -> Bot
newBot n = Bot n (-1) (-1)

updateBot :: Bot -> Int -> Bot
updateBot (Bot n l r) val
  | l /= -1 && r /= -1 = error "Cannot update bot"
  | l == -1 = Bot n val r
  | l > val = Bot n val l
  | otherwise = Bot n l val

updateBotGiving :: Bot -> Bot
updateBotGiving (Bot n _ _) = Bot n (-1) (-1)

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
parseBotInput string = (read bot, first, second)
  where
    dropped = takeWhile (/= ' ') (drop 4 string)
    bot = takeWhile (/= ' ') dropped
    first = init $ takeWhile (/= 'a') $ drop 14 $ dropWhile isDigit (drop 4 string)
    second = drop 8 $ dropWhile (/= 'h') string

initializeBots :: [Bot] -> [ValueInput] -> [Bot]
initializeBots b [] = b
initializeBots bots ((valInp, toBotInp) : xs) = initializeBots (bots ++ [toAdd]) xs
  where
    found = [b | b <- bots, toBotInp == number b]
    toAdd =
      if null found
        then Bot toBotInp valInp (-1)
        else updateBot (head found) valInp

getInputBots :: [ValueInput] -> [Bot]
getInputBots valuesInp = removeDups $ initializeBots [] valuesInp

removeDups :: [Bot] -> [Bot]
removeDups bots = removeDuplicates (reverse bots) []

removeDuplicates :: [Bot] -> [Bot] -> [Bot]
removeDuplicates [] b = b
removeDuplicates (x : xs) withoutDup =
  if botContains withoutDup x
    then removeDuplicates xs withoutDup
    else removeDuplicates xs (withoutDup ++ [x])

botContains :: [Bot] -> Bot -> Bool
botContains list (Bot bn _ _) = not (null ([b | b@(Bot xn _ _) <- list, xn == bn]))

actionOnSingleBot :: Bot -> Int -> Int -> Int -> Int -> Int -> Bot
actionOnSingleBot bot from lowTo lowValue highTo highValue
  | number bot == from = updateBotGiving bot
  | number bot == lowTo = updateBot bot lowValue
  | number bot == highTo = updateBot bot highValue
  | otherwise = bot

doAction :: [Bot] -> BotInput -> [Bot]
doAction bots action = updatedBots
  where
    (from, lowTo, highTo) = action
    fromBot = head [b | b <- bots, number b == from]
    fromNum = number fromBot
    lowToBotNum = if "bot " `isPrefixOf` lowTo then read (drop 4 lowTo) :: Int else -1
    highToBotNum = if "bot " `isPrefixOf` highTo then read (drop 4 highTo) :: Int else -1
    lowToOutputNum = if "output " `isPrefixOf` lowTo then read (drop 7 lowTo) :: Int else -1
    highToOutputNum = if "output " `isPrefixOf` highTo then read (drop 7 highTo) :: Int else -1
    lowValue = low fromBot
    highValue = high fromBot
    existingUpdatedBots = [actionOnSingleBot x fromNum lowToBotNum lowValue highToBotNum highValue | x <- bots]
    newCreatedBot1
      | lowToBotNum == (-1) = []
      | botContains existingUpdatedBots (newBot lowToBotNum) = []
      | otherwise = [Bot lowToBotNum lowValue (-1)]
    newCreatedBot2
      | highToBotNum == (-1) = []
      | botContains existingUpdatedBots (newBot highToBotNum) = []
      | otherwise = [Bot highToBotNum highValue (-1)]
    newOutput1 = [Bot (-2) lowToOutputNum lowValue | lowToOutputNum /= -1]
    newOutput2 = [Bot (-2) highToOutputNum highValue | highToOutputNum /= -1]
    updatedBots = existingUpdatedBots ++ newCreatedBot1 ++ newCreatedBot2 ++ newOutput1 ++ newOutput2

doActions2 :: [Bot] -> [BotInput] -> [Bot]
doActions2 b [] = b
doActions2 bots (a : as) =
  if not (null bot)
    then doActions2 updatedBots as
    else doActions2 bots $ as ++ [a]
  where
    bot = [b | b <- bots, canBotDoAction b a]
    updatedBots = doAction bots a

canBotDoAction :: Bot -> BotInput -> Bool
canBotDoAction bot (from, _, _) = number bot == from && low bot /= (-1) && high bot /= (-1)

findNum :: [Bot] -> [BotInput] -> Int -> Int -> Int
findNum _ [] _ _ = -1
findNum bots (x : xs) first second
  | found /= (-1) = found
  | not (null bot) = findNum updatedBots xs first second
  | otherwise = findNum bots (xs ++ [x]) first second
  where
    compare = [number b | b <- bots, low b == first && high b == second]
    found = if not (null compare) then head compare else - 1
    bot = [b | b <- bots, canBotDoAction b x]
    updatedBots = doAction bots x

part1Answer :: String -> Int -> Int -> Int
part1Answer input = findNum bots actions
  where
    parsedInput = parseInput $ lines input
    valuesInp = lefts parsedInput
    actions = rights parsedInput
    bots = getInputBots valuesInp

part2 :: String -> Int
part2 input = answer
  where
    parsedInput = parseInput $ lines input
    valuesInp = lefts parsedInput
    actions = rights parsedInput
    bots = getInputBots valuesInp
    after = doActions2 bots actions
    filtered = [(low b, high b) | b <- after, number b == -2]
    answer = product [y | (x, y) <- filtered, x `elem` [0, 1, 2]]

main :: IO ()
main = do
  input <- readFile "inputs/day-10.txt"
  putStrLn $ "Part 1 = " ++ show (part1Answer input 17 61)
  putStrLn $ "Part 2 = " ++ show (part2 input)
