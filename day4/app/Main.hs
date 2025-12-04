{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Data.List (maximumBy, nub, sort, sortBy, (!?))
import Data.Maybe (fromMaybe, mapMaybe, listToMaybe)
import Data.Ord (Down (Down), comparing)
import Debug.Trace (trace)

expect a b
  | a == b = pure ()
  | otherwise = error ("Expected " ++ show a ++ " to equal " ++ show b)

main :: IO ()
main = do
  input <- readFile "./input.txt"
  test <- readFile "./test1.txt"
  expect (part1 test) 13
  print (part1 input)
  expect (part2 test) 43
  print (part2 input)

parse :: String -> [[Int]]
parse txt =
  let grid =
        map
          ( map
              ( \case
                  '.' -> 0
                  '@' -> 1
                  _ -> error "Got some weird character"
              )
          )
          $ lines txt
   in grid

pickup a [b, x, c] d = if x == 0 then (0, 0) else let s = sum a + b + c + sum d in if s < 4 then (1, 0) else (0, x)

triples (a : b : c : xs) = [a, b, c] : triples (b: c : xs)
triples _ = []

findSpots :: [[Int]] -> (Int, [[Int]])
findSpots grid = go [] 0 (length grid - 1)
  where
    emptyRow = repeat 0
    go :: [[Int]] -> Int -> Int -> (Int, [[Int]])
    go grid' acc (-1) = (acc, grid')
    go grid' acc n =
      -- We pad the whole grid with 0s so that it's easier to move the window through it
      let top = 0 : fromMaybe emptyRow (grid !? (n - 1)) ++ [0]
          middle = 0 : fromMaybe emptyRow (grid !? n) ++ [0]
          bottom = 0 : fromMaybe emptyRow (grid !? (n + 1)) ++ [0]
          (rolls, row) = unzip $ zipWith3 pickup (triples top) (triples middle) (triples bottom)
       in go (row : grid') (sum rolls + acc) (n - 1)

part1 :: String -> Int
part1 txt =
  let grid = parse txt
   in fst $ findSpots grid

part2 :: String -> Int
part2 txt =
  let grid = parse txt
   in sum $
        map fst $
          takeWhile (\(x, _) -> x /= 0) $
            iterate (\(_, grid) -> findSpots grid) (findSpots grid)
