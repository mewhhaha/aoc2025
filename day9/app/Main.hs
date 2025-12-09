module Main (main) where

import Data.List (elemIndex, nub, sort, tails)
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
  expect (part1 test) 50
  print (part1 input)
  expect (part2 test) 24
  print (part2 input)

parse :: String -> [(Int, Int)]
parse txt = map parseTuple $ lines txt
  where
    parseTuple row =
      case elemIndex ',' row of
        (Just n) -> (read $ take n row, read $ drop (n + 1) row)
        Nothing -> error "expected coord"

size ((x1, y1), (x2, y2)) = (abs (x1 - x2) + 1) * (abs (y1 - y2) + 1)

part1 txt =
  let coords = parse txt
      pairs = [(a, b) | (a : rest) <- tails coords, b <- rest]
   in maximum $ map size pairs

-- Build vertical edges (x, yLow, yHigh) from ordered polygon vertices.
verticals :: [(Int, Int)] -> [(Int, Int, Int)]
verticals coords =
  [ (x1, min y1 y2, max y1 y2)
  | (x1, y1) : rest <- tails coords,
    (x2, y2) <- rest,
    x1 == x2
  ]

-- Pair up sorted intersection xs into inclusive x-intervals.
intervals :: [Int] -> [(Int, Int)]
intervals (a : b : rest) = (min a b, max a b) : intervals rest
intervals _ = []

part2 txt =
  let coords = parse txt
      ys = sort (nub (map snd coords))
      bands = [(y0, y1) | (y0 : y1 : _) <- tails ys, y1 > y0]
      edges = verticals coords
      bandersnatch =
        [ (y0, y1, intervals (nub (sort [x | (x, lo, hi) <- edges, lo < y1 && hi > y0])))
        | (y0, y1) <- bands,
          y1 > y0
        ]
      covers ints l r = any (\(a, b) -> a <= l && b >= r) ints
      inside (l, r, t, b) =
        all (\(y0, y1, ints) -> not (y1 > t && y0 < b) || covers ints l r) bandersnatch
      pairs = [(a, b) | (a : rest) <- tails coords, b <- rest]
      rects =
        [ (l, r, t, b, size (_1, _2))
        | (_1@(x1, y1), _2@(x2, y2)) <- pairs,
          let l = min x1 x2,
          let r = max x1 x2,
          let t = min y1 y2,
          let b = max y1 y2,
          inside (l, r, t, b)
        ]
   in maximum (map (\(_, _, _, _, s) -> s) rects)
