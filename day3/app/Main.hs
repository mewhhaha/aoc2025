module Main (main) where

import Data.List (nub, sort, sortBy, maximumBy)
import Data.Maybe (mapMaybe)
import Debug.Trace (trace)
import Data.Ord (comparing, Down (Down))

expect a b
  | a == b = pure ()
  | otherwise = error ("Expected " ++ show a ++ " to equal " ++ show b)

main :: IO ()
main = do
  input <- readFile "./input.txt"
  test <- readFile "./test1.txt"
  expect (part1 test) 357
  print (part1 input)
  expect (part2 test) 3121910778619
  print (part2 input)

parse :: String -> [[Int]]
parse txt = map (map (\x -> read [x])) $ lines txt



greatestLeftmost (a, i) (b, j)
  | a > b = GT
  | a == b && i < j = GT
  | otherwise = LT


largest 0 _ = 0
largest n row =
  let row' = zip row [0..]
      max = maximumBy greatestLeftmost
      size = length row'
      (v, i) =  max $ take (size - n  +  1) row' -- Skip last since we need two numbers
  in v * (10 ^ (n - 1)) + largest (n - 1) (drop (i + 1) row)

part1 :: String -> Int
part1 txt =
  let rows = parse txt
      joltages = map (largest 2) rows
   in sum joltages

part2 :: String -> Int
part2 txt =
  let rows = parse txt
      joltages = map (largest 12) rows
   in sum joltages
