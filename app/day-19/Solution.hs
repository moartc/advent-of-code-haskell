import qualified Control.Arrow as Data.Bifunctor

step :: ([Int], Bool) -> ([Int], Bool)
step (x : xs, delete) = if delete then step (xs, not delete) else Data.Bifunctor.first (x :) $ step (xs, not delete)
step ([], delete) = ([], delete)

stepAll :: ([Int], Bool) -> Int
stepAll inp
  | length (fst inp) == 1 = head $ fst inp
  | otherwise = stepAll $ step inp  

part1 :: Int -> Int
part1 input = stepAll (l, False)
  where
    l = [1 .. input]

main :: IO ()
main = do
  putStrLn $ "Part 1 = "  ++ show (part1 3005290)
  putStrLn $ "Part 2 = " -- ++ show (part2 )
