import qualified Data.List as List (head, concat, null, dropWhile, span, repeat, intersect)
import qualified Data.Map as Map (Map, fromList, map, keys, filter)
import qualified Data.Char as Char (isDigit)

type Vector = (Int, Int)
type Schematic = [(Int, [(Int, Char)])] 
type Symbol = Char

schematic :: String -> Schematic
schematic input = zip [0..] (map (zip [0..]) (lines input))

symbols :: Schematic -> Map.Map Vector Symbol
symbols schematic = Map.fromList (List.concat (map transform schematic))
  where transform (y, line) = map (\(x, symbol) -> ((y, x), symbol)) (filter (isSymbol . snd) line)
        isSymbol c = (not . Char.isDigit) c && c /= '.'

nums :: Schematic -> [(Vector, Int)]
nums schematic = List.concat $ map nums' schematic
  where nums' (y, line) = 
          case List.span (Char.isDigit . snd) (List.dropWhile (not . Char.isDigit . snd) line) of
            ([], []) -> []
            ([], remaining) -> nums' (y, remaining)
            (xs, remaining) -> [num xs y] ++ nums' (y, remaining)
        num xs y = ((y, (fst . List.head) xs), read (map snd xs) :: Int)

neighbours :: (Vector, Int) -> [Vector]
neighbours ((y, x), num) = 
  zip (List.repeat (y - 1)) [lowerbound .. upperbound] ++
  zip (List.repeat (y + 0)) [lowerbound .. upperbound] ++
  zip (List.repeat (y + 1)) [lowerbound .. upperbound]
    where lowerbound = x - 1
          upperbound = x + ((length . show) num)

parts :: String -> Int
parts input = sum $ map snd $ filter (\(vector, num) -> (intersects (neighbours (vector, num)) symbols')) nums'
  where schematic' = schematic input
        symbols' = Map.keys $ symbols schematic'
        nums' = nums schematic'

gears :: String -> Int
gears input = sum $ map product (
  filter (\x -> (length x) == 2) 
    (map (\gear -> map snd $ 
      filter (\((y, x), num) -> (intersects (neighbours (gear, 0)) (neighbours' y x num))) nums') gears'))
  where schematic' = schematic input
        gears' = Map.keys $ Map.filter (=='*') (symbols schematic')
        nums' = nums schematic'
        neighbours' y x num = zip (List.repeat y) [x..(x - 1 + (length . show) num)]

intersects :: (Eq a) => [a] -> [a] -> Bool
intersects a b = (not . List.null) (List.intersect a b)

main = interact (\input -> (show (parts input)) ++ "\n" ++ (show (gears input)))

