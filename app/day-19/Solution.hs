import qualified Control.Arrow as Data.Bifunctor
import Data.Sequence (Seq, deleteAt, fromList, index)

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

p2Step :: Seq Int -> Int -> Int -> Int
p2Step arr idx del
  | length arr == 1 = arr `index` 0
  | otherwise = p2Step updatedArr nextId (del -1)
  where
    idxToDel = (idx + (del `div` 2)) `mod` del
    nextId = if idxToDel > idx then idx + 1 else idx `mod` (del -1)
    updatedArr = deleteAt idxToDel arr

part2 :: Int -> Int
part2 inp = p2Step (fromList [1 .. inp]) 0 $ inp

main :: IO ()
main = do
  putStrLn $ "Part 1 = " ++ show (part1 3005290)
  putStrLn $ "Part 2 = " ++ show (part2 3005290)
