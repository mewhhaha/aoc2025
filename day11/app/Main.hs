module Main (main) where

import Data.List (elemIndex, find, nub, sort, sortBy, tails)
import Data.Maybe (fromJust, fromMaybe, isNothing, listToMaybe, mapMaybe)
import Data.Ord (comparing)
import Debug.Trace (trace)

expect a b
  | a == b = print "Passed"
  | otherwise = error ("Expected " ++ show a ++ " to equal " ++ show b)

debug x = trace (show x) x

main :: IO ()
main = do
  input <- readFile "./input.txt"
  test1 <- readFile "./test1.txt"
  test2 <- readFile "./test2.txt"
  expect (part1 test1) 5
  print (part1 input)
  expect (part2 test2) 2
  print (part2 input)

parse = map (node . words) . lines
  where
    node (w : ws) = (init w, ws)

paths current goal nodes
  | current == goal = 1
  | otherwise =
      let outputs = fromJust $ lookup current nodes
       in sum $ map (\x -> paths x goal nodes) outputs

part1 txt =
  let nodes = parse txt
   in paths "you" "out" nodes

dacfft path = "dac" `elem` path && "fft" `elem` path

opaths :: String -> [(String, Int)] -> [(String, [String])] -> (Int, [(String, Int)])
opaths current visited nodes =
  case lookup current visited of
    Just xs -> (xs, visited)
    Nothing ->
      let outputs = fromJust $ lookup current nodes
          (count, visited') = foldl' (\(n, vs) x -> let (m, vs') = opaths x vs nodes in (m + n, vs')) (0, visited) outputs
       in (count, (current, count) : visited')

part2 txt =
  let nodes = parse txt
      dac2out = fst $ opaths "dac" [("out", 1)] nodes
      fft2dac = fst $ opaths "fft" [("dac", 1), ("out", 0)] nodes
      src2fft = fst $ opaths "svr" [("fft", 1), ("out", 0)] nodes
   in src2fft * fft2dac * dac2out
