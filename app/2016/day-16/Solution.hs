step :: String -> String
step s = s ++ ['0'] ++ changeBits (reverse s)

changeBits :: String -> String
changeBits xs = [if x == '1' then '0' else '1' | x <- xs]

generate :: String -> Int -> String
generate input l = if length input >= l then take l input else generate (step input) l

getGroups :: [Char] -> [String]
getGroups (a : b : xs) = (a : [b]) : getGroups xs
getGroups _ = []

generateCheckSum :: String -> String
generateCheckSum input = [if g == "11" || g == "00" then '1' else '0' | g <- getGroups input]  

getChecksum :: String -> String
getChecksum inp = if odd (length inp) then inp else getChecksum $ generateCheckSum inp

solve :: String -> Int -> String
solve input = getChecksum . generate input

main :: IO ()
main = do
  putStrLn $ "Part 1 = " ++ show (solve "10111011111001111" 272)
  putStrLn $ "Part 2 = " ++ show (solve "10111011111001111" 35651584)
