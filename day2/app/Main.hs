module Main (main) where

import Data.List (nub)
import Data.Maybe (mapMaybe)
import Debug.Trace (trace)

expect a b
  | a == b = pure ()
  | otherwise = error ("Expected " ++ show a ++ " to equal " ++ show b)

main :: IO ()
main = do
  input <- readFile "./input.txt"
  test <- readFile "./test1.txt"
  expect (part1 test) 1227775554
  print (part1 input)
  expect (part2 test) 4174379265
  print (part2 input)

split c = split_ c ""

split_ c "" "" = []
split_ c y "" = [reverse y]
split_ c y (x : xs)
  | c == x = reverse y : split_ c "" xs
  | otherwise = split_ c (x : y) xs

parse :: String -> [(Int, Int)]
parse = map range . split ','
  where
    range s = case split '-' s of
      [a, b] -> (read a, read b)
      _ -> error "wow"

chunkOf n [] = []
chunkOf n xs =
  let (a, b) = splitAt n xs
   in a : chunkOf n b

repeats :: Int -> Int -> Maybe Int
repeats n s =
  let digits = show s
      size = length digits
      chunks = chunkOf (size `div` n) digits
      first = case chunks of
        [] -> error "empty"
        (x : xs) -> x
   in if size `mod` n /= 0 then Nothing else if length chunks == n && all (== first) chunks then Just s else Nothing

range (a, b) = [a .. b]

part1 file =
  let ranges = parse file
      invalids = concatMap (mapMaybe (repeats 2) . range) ranges
   in sum invalids

repeatsAny :: Int -> Maybe Int
repeatsAny s =
  let digits = show s
      size = length digits
      divisors = [2 .. size]
   in if null (mapMaybe (`repeats` s) divisors) then Nothing else Just s

part2 file =
  let ranges = parse file
      invalids = concatMap (mapMaybe repeatsAny . range) ranges
   in sum invalids
