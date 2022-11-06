import Data.Digest.Pure.MD5 (md5)
import Data.List (isInfixOf)
import Data.String (IsString (fromString))

input :: [Char]
input = "ahsbgdzn"

hash :: [String]
hash = [show $ md5 $ fromString (input ++ show index) | index <- [0 ..]]

hashStr :: String -> String
hashStr input = show $ md5 $ fromString input

getTrippleChar5 :: String -> String
getTrippleChar5 (a : b : c : xs)
  | a == b && b == c = replicate 5 a
  | otherwise = getTrippleChar5 ([b] ++ [c] ++ xs)
getTrippleChar5 _ = []

next1000Contain :: Int -> String -> Bool
next1000Contain currentIdx substring = any (isInfixOf substring) [hash !! idx | idx <- [currentIdx + 1 .. currentIdx + 1000]]

hashIdx :: [(String, Int)]
hashIdx = filter (\(h, i) -> next1000Contain i (getTrippleChar5 h)) [(hsh, idx) | idx <- [0 ..], let hsh = hash !! idx, getTrippleChar5 hsh /= ""]

part1 :: Int
part1 = snd $ hashIdx !! 64

----- PART 2 -----
stretchedHash :: String -> String
stretchedHash hash = foldl (\h x -> hashStr h) hash [x | x <- [1 .. 2016]]

next1000Contain2 :: Int -> String -> Bool
next1000Contain2 currentIdx substring = any (isInfixOf substring) [hashList !! idx | idx <- [currentIdx + 1 .. currentIdx + 1000]]

hashList :: [String]
hashList = [stretchedHash $ hash !! idx | idx <- [0 ..]]

hashIdx2 :: [(String, Int)]
hashIdx2 = [(h, idx) | idx <- [0 ..], let h = hashList !! idx, let tripple = getTrippleChar5 h, tripple /= "", next1000Contain2 idx tripple]

part2 :: Int
part2 = snd $ hashIdx2 !! 64

main :: IO ()
main = do
  putStrLn $ "Part 1 = " ++ show part1
  putStrLn $ "Part 2 = " ++ show part2
