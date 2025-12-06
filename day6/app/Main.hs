module Main (main) where

import Data.List (sortOn, transpose)
import Debug.Trace (trace)

type Range = (Int, Int)

expect :: (Eq a, Show a, Applicative f) => a -> a -> f ()
expect a b
  | a == b = pure ()
  | otherwise = error ("Expected " ++ show a ++ " to equal " ++ show b)

main :: IO ()
main = do
  input <- readFile "./input.txt"
  test <- readFile "./test1.txt"
  expect (part1 test) 4277556
  print (part1 input)
  expect (part2 test) 3263827
  print (part2 input)

data Problem = Problem (Int -> Int -> Int) [Int]

parseOp "*" = (*)
parseOp "+" = (+)

parse :: String -> [Problem]
parse = map problem . transpose . map words . lines
  where
    problem xs = case reverse xs of
      (x : xs) -> Problem (parseOp x) (map read xs)

part1 :: String -> Int
part1 txt =
  let problems = parse txt
      solve = map $ \(Problem op is) -> foldl1 op is
   in sum $ solve problems

split :: [String] -> [[String]]
split = go [] []
  where
    go acc c [] = reverse (reverse c : acc)
    go acc c (x : xs) = if filter (/= ' ') x == "" then go (reverse c : acc) [] xs else go acc (x : c) xs

debug x = trace x x

parse2 :: String -> [Problem]
parse2 txt =
  let rows = lines txt
      operators = map parseOp $ words $ last rows
      numbers =
        map (map (read . filter (/= ' '))) $
          split $
            transpose $
              init rows
   in zipWith Problem operators (trace (show numbers) numbers)

part2 :: String -> Int
part2 txt =
  let problems = parse2 txt
      solve = map $ \(Problem op is) -> foldl1 op is
   in sum $ solve problems
