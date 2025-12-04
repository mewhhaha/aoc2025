{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Data.List (maximumBy, nub, sort, sortBy, (!?))
import Data.Maybe (mapMaybe)
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

data Cell = Empty | PaperRoll
  deriving (Eq)

parse :: String -> ([Cell], (Int, Int))
parse txt =
  let grid =
        map
          ( map
              ( \case
                  '.' -> Empty
                  '@' -> PaperRoll
                  _ -> error "Got some weird character"
              )
          )
          $ lines txt
      w = length grid
      h =
        ( case grid of
            [] -> 0
            (x : _) -> length x
        )
   in (concat grid, (w, h))

kernel x (w, h)
  | x `mod` w == 0 =
      [ x + 1,
        x - w,
        x - w + 1,
        x + w,
        x + w + 1
      ]
  | x `mod` w == w - 1 =
      [ x - 1,
        x - w,
        x - w - 1,
        x + w,
        x + w - 1
      ]
  | otherwise =
      [ x - 1,
        x + 1,
        x - w,
        x - w - 1,
        x - w + 1,
        x + w,
        x + w - 1,
        x + w + 1
      ]

findSpots grid size =
  let (spots, grid') = unzip $ zipWith go [0 ..] grid
   in (sum spots, grid')
  where
    go i x =
      if x == Empty
        then (0, Empty)
        else case filter ((Just PaperRoll ==) . (grid !?)) (kernel i size) of
          (_1 : _2 : _3 : _4 : _) -> (0, PaperRoll)
          _ -> (1, Empty)

part1 :: String -> Int
part1 txt =
  let (grid, size) = parse txt
   in fst $ findSpots grid size

part2 :: String -> Int
part2 txt =
  let (grid, size) = parse txt
   in go 0 grid size
  where
    go acc grid size =
      let (empty, grid') = findSpots grid size
       in case empty of
            0 -> acc
            n -> go (acc + n) grid' size
