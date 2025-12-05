module Main (main) where

import Data.Bifunctor (bimap)
import Data.List (isPrefixOf, maximumBy, nub, sort, sortBy, (!?))
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Ord (Down (Down), comparing)
import Debug.Trace (trace)

expect a b
  | a == b = pure ()
  | otherwise = error ("Expected " ++ show a ++ " to equal " ++ show b)

main :: IO ()
main = do
  input <- readFile "./input.txt"
  test <- readFile "./test1.txt"
  expect (part1 test) 3
  print (part1 input)
  expect (part2 test) 14
  print (part2 input)

split :: (Eq a) => a -> [a] -> ([a], [a])
split q xs = go [] q xs
  where
    go left q w@(x : xs) = if q == x then (reverse left, xs) else go (x : left) q xs
    go left _ [] = (reverse left, [])

parse :: String -> ([(Int, Int)], [Int])
parse txt =
  let (rawRanges, rawInstructions) = split [] (lines txt)
      ranges = map (bimap read read . split '-') rawRanges
      instructions = map read rawInstructions
   in (ranges, instructions)

part1 :: String -> Int
part1 txt =
  let (ranges, ingredients) = parse txt
      isInRange x = any (\(a, b) -> x >= a && x <= b) ranges
   in length $ filter isInRange ingredients

data Collision = Split Int Int | LowerTo Int | RaiseTo Int | Remove
  deriving (Show)

part2 :: String -> Int
part2 txt =
  let (ranges, _) = parse txt
      uniq = foldl' go [] ranges
   in sum $ map (\(a, b) -> b - a + 1) uniq
  where
    go acc (a, b) =
      let collision =
            listToMaybe $
              mapMaybe
                ( \(x, y) ->
                    let bottomIn = x >= a && x <= b
                        topIn = y >= a && y <= b
                     in if bottomIn && topIn
                          then Just (Split (x - 1) (y + 1))
                          else
                            if bottomIn
                              then Just (LowerTo (x - 1))
                              else
                                if topIn
                                  then Just (RaiseTo (y + 1))
                                  else
                                    if a >= x && b <= y
                                      then Just Remove
                                      else Nothing
                )
                acc
       in case trace (show collision) collision of
            Just (LowerTo x) -> go acc (a, x)
            Just (RaiseTo y) -> go acc (y, b)
            Just (Split x y) -> go (go acc (a, x)) (y, b)
            Just Remove -> acc
            Nothing -> (a, b) : acc
