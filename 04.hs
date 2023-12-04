import qualified Data.List as List (intersect)
import qualified Data.Map as Map (Map, fromList, map, keys, mapWithKey, (!))
import qualified Data.Text as Text (pack, unpack, splitOn, null)

card :: String -> (Int, ([Int], [Int]))
card line = (id, (winners, mine))
  where components = Text.splitOn (Text.pack ": ") (Text.pack (drop 5 line))
        id = read (Text.unpack (components !! 0)) :: Int
        numbers = Text.splitOn (Text.pack " | ") (components !! 1)
        winners = nums (numbers !! 0)
        mine = nums (numbers !! 1)
        nums text = map (\x -> read (Text.unpack x) :: Int) (filter (not . Text.null) (Text.splitOn (Text.pack " ") (text)))

fold' :: [Int] -> Int
fold' [] = 0
fold' xs = 2 ^ ((length xs) - 1)

play :: [Int] -> Map.Map Int [Int] -> Int
play [] _ = 0
play (x:xs) resolved = (length cards) + (play cards resolved) + (play xs resolved)
  where cards = resolved Map.! x

main = do
  input <- getContents

  let cards = Map.fromList $ map card (lines input)
  let resolved = Map.mapWithKey (\id (winners, mine) 
        -> map (\i -> (i + id)) (map fst (zip [1..] (List.intersect mine winners)))) cards

  print $ sum $ (Map.map fold' resolved)
  print $ sum $ map (\id -> 1 + (play [id] resolved)) (Map.keys cards)
