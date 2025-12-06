module Main (main) where

import Data.List (transpose)

expect :: (Eq a, Show a, Applicative f) => a -> a -> f ()
expect a b
  | a == b = pure ()
  | otherwise = error ("Expected " ++ show a ++ " to equal " ++ show b)

main :: IO ()
main = do
  input <- readFile "./input.txt"
  test <- readFile "./test1.txt"
  expect (part1 test) 4277556
  print (part1 input)
  expect (part2 test) 3263827
  print (part2 input)

data Problem = Problem (Int -> Int -> Int) [Int]

parseOp :: String -> (Int -> Int -> Int)
parseOp "*" = (*)
parseOp "+" = (+)
parseOp op = error ("Unknown operator: " ++ op)

solve :: [Problem] -> Int
solve = sum . map (\(Problem op xs) -> foldl1 op xs)

parse1 :: String -> [Problem]
parse1 = map toProblem . transpose . map words . lines
  where
    toProblem col = case reverse col of
      rawOp : nums -> Problem (parseOp rawOp) (map read nums)
      _ -> error "Empty column encountered while parsing"

part1 :: String -> Int
part1 = solve . parse1

splitColumns :: [String] -> [[String]]
splitColumns = go [] []
  where
    go acc current [] = reverse (if null current then acc else reverse current : acc)
    go acc current (c : cs)
      | all (== ' ') c =
          let acc' = if null current then acc else reverse current : acc
           in go acc' [] cs
      | otherwise = go acc (c : current) cs

parse2 :: String -> [Problem]
parse2 txt =
  let rows = lines txt
      ops = map parseOp . words $ last rows
      columns = transpose (init rows)
      numbers = map read <$> splitColumns columns
   in zipWith Problem ops numbers

part2 :: String -> Int
part2 = solve . parse2
