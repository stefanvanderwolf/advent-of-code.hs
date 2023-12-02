import qualified Data.List as List (tails, isPrefixOf, find, all, sortOn, concat)
import qualified Data.Map as Map (Map, fromList, toList, union, lookup, map)
import qualified Data.Maybe as Maybe (mapMaybe, fromJust)
import qualified Data.Text as Text (splitOn, pack, unpack)
import qualified Data.Tuple as Tuple (swap)

game :: String -> (Int, [[(Int, String)]])
game x = (read (components !! 0), draws (components !! 1))
  where components = map Text.unpack (Text.splitOn (Text.pack ": ") (Text.pack (drop 5 x)))

draws :: String -> [[(Int, String)]]
draws x = map (draw . Text.unpack) (Text.splitOn (Text.pack "; ") (Text.pack x))

draw :: String -> [(Int, String)]
draw x = map (cube . Text.unpack) (Text.splitOn (Text.pack ", ") (Text.pack x))

cube :: String -> (Int, String)
cube x = (read (components !! 0), components !! 1)
  where components = map Text.unpack (Text.splitOn (Text.pack " ") (Text.pack x))

possible :: Map.Map String Int -> (Int, [[(Int, String)]]) -> Maybe Int
possible bag (id, draws) = if all possible' draws then Just id else Nothing
  where possible' draw = List.all (\(amount, cube) -> amount <= (Maybe.fromJust (Map.lookup cube bag))) draw

fewest :: (Int, [[(Int, String)]]) -> Int
fewest game = product $ map snd (Map.toList (Map.fromList (List.sortOn snd (map Tuple.swap draws))))
  where draws = List.concat (snd game)

main = do
  input <- getContents

  let bag = Map.fromList [("red", 12), ("blue", 14), ("green", 13)]

  print $ sum $ Maybe.mapMaybe (possible bag) (map game (lines input))
  print $ sum $ map (fewest . game) (lines input)
