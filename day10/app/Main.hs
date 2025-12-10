module Main (main) where

import Data.List (elemIndex, find, nub, sort, sortBy, tails)
import Data.Maybe (isNothing)
import Data.Ord (comparing)
import Debug.Trace (trace)

expect :: (Eq a, Show a, Applicative f) => a -> a -> f ()
expect a b
  | a == b = pure ()
  | otherwise = error ("Expected " ++ show a ++ " to equal " ++ show b)

debug x = trace (show x) x

main :: IO ()
main = do
  input <- readFile "./input.txt"
  test <- readFile "./test1.txt"
  expect (part1 test) 7
  print (part1 input)
  expect (part2 test) 24
  print (part2 input)

data Diagram = Diagram [Bool] [[Int]] [Int]
  deriving (Show, Eq)

parse txt = map (foldl' go (Diagram [] [] []) . words) $ lines txt
  where
    go (Diagram s l r) row =
      case row of
        ('(' : xs) -> Diagram s (l ++ [read ("[" ++ init xs ++ "]")]) r
        ('[' : xs) -> Diagram (map (== '#') $ init xs) l r
        ('{' : xs) -> Diagram s l (read $ "[" ++ init xs ++ "]")

push button s = zipWith (\c i -> if i `elem` button then not c else c) s [0 ..]

shortest :: Diagram -> Int
shortest (Diagram final l _) = go (map (\x -> (empty, [x])) l)
  where
    empty = map (const False) final
    go ((lights, prev@(button : _)) : xs) =
      let variations = map (\x -> (lights', x : prev)) $ filter (/= button) l
          queue' = sortBy (comparing (length . snd)) (xs ++ variations)
          lights' = push button lights
       in case any (\x -> fst x == lights') xs of
            _ | lights' == final -> length prev
            False -> go queue'
            True -> go xs

part1 txt =
  let diagrams = parse txt
   in sum $ debug $ map shortest diagrams

part2 txt = 2
