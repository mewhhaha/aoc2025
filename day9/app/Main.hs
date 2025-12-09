module Main (main) where

import Data.List (elemIndex, tails)
import Debug.Trace (trace)
import Data.Bits (Bits(xor))

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

inside :: [(Int, Int)] -> (Int, Int) -> Bool
inside poly (px, py) =
  let closed = poly ++ take 1 poly
      onSegment (x1, y1) (x2, y2) =
        let cross = (py - y1) * (x2 - x1) - (px - x1) * (y2 - y1)
            withinX = px >= min x1 x2 && px <= max x1 x2
            withinY = py >= min y1 y2 && py <= max y1 y2
         in cross == 0 && withinX && withinY
      crosses (x1, y1) (x2, y2) =
        let cond = (y1 > py) /= (y2 > py)
            xInt = x1 + (py - y1) * (x2 - x1) `div` (y2 - y1)
         in cond && px < xInt
      step acc (a : b : rest)
        | onSegment a b = True
        | otherwise = step (acc `xor` crosses a b) (b : rest)
      step acc _ = acc
   in step False closed


crosses :: [(Int, Int)] -> (Int, Int, Int, Int) -> Bool
crosses poly (l, r, t, b) =
  let closed = poly ++ take 1 poly
      overlap a1 a2 b1 b2 =
        let lo = max a1 b1
            hi = min a2 b2
         in hi - lo > 0
      vertical (x1, y1) (x2, y2)
        | x1 /= x2 = False
        | otherwise =
            let x = x1
                yLow = min y1 y2
                yHigh = max y1 y2
             in x > l && x < r && overlap yLow yHigh t b
      horizontal (x1, y1) (x2, y2)
        | y1 /= y2 = False
        | otherwise =
            let y = y1
                xLow = min x1 x2
                xHigh = max x1 x2
             in y > t && y < b && overlap xLow xHigh l r
      go (a : b' : rest) = vertical a b' || horizontal a b' || go (b' : rest)
      go _ = False
   in go closed

process :: [(Int, Int)] -> [((Int, Int), (Int, Int))] -> [((Int, Int), (Int, Int))]
process poly = filter ok
  where
    ok pair@((x1, y1), (x2, y2))
      | x1 == x2 || y1 == y2 = True
      | otherwise =
          let l = min x1 x2
              r = max x1 x2
              t = min y1 y2
              b = max y1 y2
           in all (inside poly) [(l,t), (r,t), (l,b), (r,b)]
              && not (crosses poly (l, r, t, b))

part2 txt =
  let coords = parse txt
      pairs = [(a, b) | (a : rest) <- tails coords, b <- rest]
   in maximum $ map size $ process coords pairs
