module Main (main) where

import Data.List (sortOn)

type Range = (Int, Int)

expect :: (Eq a, Show a, Applicative f) => a -> a -> f ()
expect a b
  | a == b = pure ()
  | otherwise = error ("Expected " ++ show a ++ " to equal " ++ show b)

main :: IO ()
main = do
  input <- readFile "./input.txt"
  test <- readFile "./test1.txt"
  expect (part1 test) 3
  print (part1 input)
  expect (part2 test) 14
  print (part2 input)

parse :: String -> ([Range], [Int])
parse txt =
  let (rawRanges, rest) = break null (lines txt)
      toRange (left, '-' : right) = (read left, read right)
      toRange _ = error "Invalid range"
      ranges = map (toRange . break (== '-')) rawRanges
      instructions = map read (drop 1 rest)
   in (ranges, instructions)

inRange :: Range -> Int -> Bool
inRange (a, b) x = a <= x && x <= b

part1 :: String -> Int
part1 txt =
  let (ranges, ingredients) = parse txt
   in length $ filter (\x -> any (`inRange` x) ranges) ingredients

merge :: [Range] -> [Range]
merge = foldl' step [] . sortOn fst
  where
    step [] r = [r]
    step acc@((l, h) : rs) (a, b)
      | a <= h + 1 = (l, max h b) : rs
      | otherwise = (a, b) : acc

part2 :: String -> Int
part2 txt =
  let (ranges, _) = parse txt
      uniq = merge ranges
   in sum $ map (\(a, b) -> b - a + 1) uniq
