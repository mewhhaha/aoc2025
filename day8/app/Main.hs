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

main :: IO ()
main = do
  input <- readFile "./input.txt"
  test <- readFile "./test1.txt"
  expect (part1 test 10) 40
  print (part1 input 1000)
  expect (part2 test) 25272
  print (part2 input)

split c ss = case elemIndex c ss of
  Just i -> take i ss : split c (drop (i + 1) ss)
  Nothing -> [ss]

parse :: String -> [(Int, Int, Int)]
parse txt = map coord $ lines txt
  where
    coord x = case split ',' x of
      [a, b, c] -> (read a, read b, read c)
      x -> error $ "Shoulda been three coordinates but got " ++ show x

euclidian (a1, b1, c1) (a2, b2, c2) = sqrt $ part a1 a2 + part b1 b2 + part c1 c2
  where
    part a b = (fromIntegral b - fromIntegral a) ^ 2

connect :: [(Int, Int, Int)] -> [((Int, Int, Int), (Int, Int, Int))] -> (Maybe ((Int, Int, Int), (Int, Int, Int)), [[(Int, Int, Int)]])
connect _ [] = (Nothing, [])
connect coords pairs = go (map (: []) coords) pairs
  where
    go acc [] = (Nothing, acc)
    go acc ((a, b) : rest) =
      let (x, xs) = extract [] a acc
          (y, ys) = extract [] b xs
       in case (x, y , ys) of
            (Just x, Just y, []) -> (Just (a,b), x : [y])
            (Just x, Just y, _) -> go ((x ++ y) : ys) rest
            _ -> go acc rest
    extract acc a (x : xs) = if a `elem` x then (Just x, acc ++ xs) else extract (x : acc) a xs
    extract acc _ [] = (Nothing, acc)

part1 txt n =
  let coords = parse txt
      pairs = [(a, b) | (a : rest) <- tails coords, b <- rest]
      sorted = sortBy (compare `on` uncurry euclidian) pairs
   in product $ take 3 $ sortBy (comparing Down) (map length $ snd $ connect coords $ take n sorted)

part2 txt =
  let coords = parse txt
      pairs = [(a, b) | (a : rest) <- tails coords, b <- rest]
      sorted = sortBy (compare `on` uncurry euclidian) pairs
   in case fst $ connect coords sorted of
          Just ((x1, _, _), (x2, _ ,_)) -> x1 * x2
          Nothing -> error "They should have all connected"

debug x = trace (show x) x
