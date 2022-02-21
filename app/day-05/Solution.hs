import Data.Char (isOctDigit)
import Data.Digest.Pure.MD5 (md5)
import Data.Function (on)
import Data.List (isPrefixOf, sortBy)
import Data.String (IsString (fromString))

passwd1 :: String -> String
passwd1 string = take 8 [hash !! 5 | index <- [1 ..], let hash = show $ md5 (fromString (string ++ show index)), "00000" `isPrefixOf` hash]

passwd2 :: String -> String
passwd2 input = [x | (_, x) <- sortBy (compare `on` fst) $ findPasswd input [] 0]

findPasswd :: String -> [(Char, Char)] -> Int -> [(Char, Char)]
findPasswd input arr startIdx =
  if length arr == 8
    then arr
    else findPasswd input (arr ++ [(pos, char)]) lastIdx
  where
    (pos, char, lastIdx) =
      head [(foundPos, hash !! 6, index) | index <- [startIdx + 1 ..], let hash = show $ md5 (fromString (input ++ show index)), let foundPos = hash !! 5, "00000" `isPrefixOf` hash && isOctDigit foundPos && not (any ((== foundPos) . fst) arr)]

main :: IO ()
main = do
  input <- readFile "inputs/day-05.txt"
  putStrLn $ "Part 1 = " ++ show (passwd1 input)
  putStrLn $ "Part 2 = " ++ show (passwd2 input)
