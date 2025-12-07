module Main (main) where

expect :: (Eq a, Show a, Applicative f) => a -> a -> f ()
expect a b
  | a == b = pure ()
  | otherwise = error ("Expected " ++ show a ++ " to equal " ++ show b)

main :: IO ()
main = do
  input <- readFile "./input.txt"
  test <- readFile "./test1.txt"
  expect (part1 test) 21
  print (part1 input)
  expect (part2 test) 40
  print (part2 input)

data Cell
  = Tachyon Int
  | Splitter Bool
  | Empty
  deriving (Eq)

parse :: String -> [[Cell]]
parse = map (map toCell) . lines
  where
    toCell 'S' = Tachyon 1
    toCell '^' = Splitter False
    toCell '.' = Empty
    toCell _ = Empty

simulate :: [[Cell]] -> [[Cell]]
simulate (top : cur : rest) = top : simulate (splitRow top cur : rest)
simulate rows = rows

splitRow :: [Cell] -> [Cell] -> [Cell]
splitRow top cur = map (snd . step) triples
  where
    zipped = zip top cur
    padded = (Empty, Empty) : zipped ++ [(Empty, Empty)]
    triples = zip3 padded (drop 1 padded) (drop 2 padded)

    step (left, center, right) =
      let leftBeam = case left of
            (Tachyon n, Splitter _) -> n
            _ -> 0
          rightBeam = case right of
            (Tachyon n, Splitter _) -> n
            _ -> 0
       in case center of
            (Tachyon n, Splitter _) -> (Tachyon n, Splitter True)
            (Tachyon n, Empty) -> (Tachyon n, Tachyon (n + leftBeam + rightBeam))
            (Empty, Empty)
              | leftBeam + rightBeam > 0 -> (Empty, Tachyon (leftBeam + rightBeam))
              | otherwise -> (Empty, Empty)
            x -> x

part1 :: String -> Int
part1 = length . concatMap (filter (== Splitter True)) . simulate . parse

part2 :: String -> Int
part2 = sum . map count . last . simulate . parse
  where
    count (Tachyon n) = n
    count _ = 0
