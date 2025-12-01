module Main (main) where


expect a b
  | a == b = pure ()
  | otherwise = error ("Expected " ++ show a ++ " to equal " ++ show b)

main :: IO ()
main = do
  input <- readFile "./input.txt"
  test <- readFile "./test1.txt"
  expect (part1 test) 3
  print (part1 input)
  expect (part2 test) 6
  print (part2 input)

part1 :: String -> Int
part1 file =
  let content = map parse . lines $ file
      rotations = scanl step 50 content
      zeros = length (filter (== 0) rotations)
   in zeros

step r move = (r + move) `mod` 100

parse ('L' : rest) = negate (read rest)
parse ('R' : rest) = read rest

part2 :: String -> Int
part2 file =
  let content = map parse . lines $ file
      rotations = scanl advance (0, 50) content
      clicks = sum $ map fst rotations
   in clicks

advance (_, r) move =
  let (loops, rest) = abs move `divMod` 100
      end
        | r == 0 = False
        | move < 0 = r - rest <= 0
        | otherwise = r + rest >= 100
   in (loops + fromEnum end, (r + move) `mod` 100)
