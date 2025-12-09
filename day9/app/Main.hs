module Main (main) where

import Data.Either (partitionEithers)
import Data.Function (on)
import Data.List (elemIndex, findIndex, nub, sort, sortBy, tails)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Ord
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

contains ((x1, y1), (x2, y2)) (a, b) =
  let left = min x1 x2
      right = max x1 x2
      top = min y1 y2
      bottom = max y1 y2
   in a > left && a < right && b > top && b < bottom

overlaps ((x1, y1), (x2, y2)) (a, b) =
  let left = min x1 x2
      right = max x1 x2
      top = min y1 y2
      bottom = max y1 y2
   in a >= left && a <= right && b >= top && b <= bottom

corners ((x1, y1), (x2, y2)) =
  let left = min x1 x2
      right = max x1 x2
      top = min y1 y2
      bottom = max y1 y2
   in [(left, top), (right, top), (left, bottom), (right, bottom)]

process :: [((Int, Int), (Int, Int))] -> [((Int, Int), (Int, Int))]
process = go []
  where
    go valid remaining =
      let (left, right) =
            partitionEithers $
              map
                ( \pair -> case pair of
                    ((x1, y1), (x2, y2))
                      | x1 == x2 || y1 == y2 -> Right pair
                      | all (\corner -> any (`overlaps` corner) valid) (corners pair) -> Right pair
                      | otherwise -> Left pair
                )
                remaining
       in case right of
            [] -> valid
            _ -> go (valid ++ right) left

part2 txt =
  let coords = parse txt
      pairs = [(a, b) | (a : rest) <- tails coords, b <- rest]
   in maximum $ map size $ process pairs
