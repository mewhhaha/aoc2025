module Main (main) where

import Data.Function (on)
import Data.List (elemIndex, findIndex, nub, sort, sortBy, tails)
import Data.Maybe (fromMaybe)
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

size ((x1,y1),(x2,y2)) = (abs (x1 - x2) + 1) * (abs (y1 - y2) + 1)


part1 txt =
  let coords = parse txt
      pairs = [(a,b) | (a:rest) <- tails coords, b <- rest]
  in case sortBy (comparing Down) $ map size pairs of
        (x:_) -> x
        _ -> error "empty list"

contains ((x1,y1),(x2, y2)) (a, b) = let left = min x1 x2
                                         right = max x1 x2
                                         top = min y1 y2
                                         bottom = max y1 y2
                                      in a > left && a < right && b > top && b < bottom

corners ((x1,y1), (x2, y2)) =
    let left = min x1 x2
        right = max x1 x2
        top = min y1 y2
        bottom = max y1 y2
    in ((left, top), (right, top), (left, bottom), (left, top))            
isPartOf :: [((Int, Int), (Int, Int), (Int, Int), (Int, Int))] -> [((Int, Int), (Int, Int))] -> Bool
isPartOf [] rects = True
isPartOf _ [] = False
isPartOf test rects = isPartOf (concatMap go test) rects

          where
             go all@(topLeft, topRight, bottomLeft, bottomRight) =
                      let checks = foldl' (\(a,b,c,d) rect -> (a || rect `contains` topLeft, b || rect `contains` topRight, c || rect `contains` bottomLeft, d || rect `contains` bottomRight ) ) (False, False, False, False) rects
                      in case checks of
                                (False, False, False, False ) -> [all]
                                (True, True, True, True) -> []



part2 txt =
  let coords = parse txt
      pairs = [(a,b) | (a:rest) <- tails coords, b <- rest]
      rects = [rect| rect <- pairs , not $ any (rect `contains`) coords]
  in case sortBy (comparing Down) $ map size $ filter (\x -> [corners x] `isPartOf` rects) pairs of
      (x:_) -> x
      _ -> error "empty list"
