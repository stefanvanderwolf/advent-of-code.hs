report :: String -> [[Int]]
report input = map values (lines input)
  where values line = map read (words line)

differences :: [Int] -> [[Int]]
differences values
  | all (==0) values = [values]
  | otherwise = values:(differences (difference values))
  where difference [a, b] = [b - a]
        difference (a:b:xs) = (b - a):difference (b:xs)

main = do
  input <- getContents

  let report' = (report input)
  print $ sum $ map (foldr ((+) . last) 0 . differences) report'
  print $ sum $ map (foldr ((-) . head) 0 . differences) report'
