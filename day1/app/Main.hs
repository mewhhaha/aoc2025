module Main (main) where

import Control.Exception (assert)
import Debug.Trace (trace)

data Rotation = L Int | R Int

expect a b = do
  if a == b then return () else error ("Expected " ++ show a ++ " to equal " ++ show b)

main :: IO ()
main = do
  input <- readFile "./input.txt"
  test <- readFile "./test1.txt"
  expect (part1 test) 3
  print $ part1 input
  expect (part2 test) 6
  print $ part2 input

part1 :: String -> Int
part1 file =
  let content = map parse . lines $ file
      rotations = scanl apply 50 content
      zeros = length (filter (== 0) rotations)
   in zeros

apply r (L n) = (r - n) `mod` 100
apply r (R n) = (r + n) `mod` 100

parse ('L' : rest) = L (read rest)
parse ('R' : rest) = R (read rest)
parse unknown = error unknown

part2 :: String -> Int
part2 file =
  let content = map parse . lines $ file
      rotations = scanl apply2 (0, 50) content
      clicks = sum $ map fst rotations
   in clicks

apply2 (_, r) (L n) =
  let rest = n `rem` 100
      during = n `div` 100
      end = if r /= 0 && r - rest <= 0 then 1 else 0
   in (during + end, (r - rest) `mod` 100)
apply2 (_, r) (R n) =
  let rest = n `rem` 100
      during = n `div` 100
      end = if r /= 0 && r + rest >= 100 then 1 else 0
   in (during + end, (r + rest) `mod` 100)
