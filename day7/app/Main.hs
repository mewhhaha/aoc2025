module Main (main) where

import Data.List (transpose)

expect :: (Eq a, Show a, Applicative f) => a -> a -> f ()
expect a b
  | a == b = pure ()
  | otherwise = error ("Expected " ++ show a ++ " to equal " ++ show b)

main :: IO ()
main = do
  input <- readFile "./input.txt"
  test <- readFile "./test1.txt"
  expect (part1 test) 21
  print (part1 input)
  expect (part2 test) 40
  print (part2 input)

data Cell = Tachyon Int | Splitter Bool | Empty
  deriving (Eq)

parse txt =
  -- Pad left and right with Empty so we can always rely on targetting a middle element
  let rows = map ((\x -> Empty : x ++ [Empty]) . map parseTachyon) (lines txt)
      parseTachyon 'S' = Tachyon 1
      parseTachyon '^' = Splitter False
      parseTachyon '.' = Empty
   in rows

solve acc (x : y : xs) = solve (x : acc) (split [] (zip x y) : xs)
solve acc xs = reverse acc ++ xs

split acc (a : b : c : xs) =
  let left = case a of
        (Tachyon n, Splitter _) -> n
        _ -> 0
      right = case c of
        (Tachyon n, Splitter _) -> n
        _ -> 0
      b' = case b of
        (Tachyon n, Splitter _) -> (Tachyon n, Splitter True)
        (Tachyon n, Empty) -> (Tachyon n, Tachyon (n + left + right))
        (Empty, Empty) -> (Empty, if left + right > 0 then Tachyon (left + right) else Empty)
        x -> x
   in split (snd a : acc) (b' : c : xs)
split acc xs = reverse acc ++ map snd xs

part1 txt =
  let rows = parse txt
   in length $ concatMap (filter (== Splitter True)) (solve [] rows)

part2 txt =
  let rows = parse txt
      count (Tachyon n) = n
      count _ = 0
      timelines = sum $ map count $ last (solve [] rows)
   in timelines
