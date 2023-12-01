import qualified Data.List as List (tails, isPrefixOf, find)
import qualified Data.Map as Map (Map, fromList, toList, union)
import qualified Data.Maybe as Maybe (mapMaybe)

words' :: Map.Map String Int
words' = Map.fromList $ zip ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"] [1 .. 9]

digits' :: Map.Map String Int
digits' = Map.fromList $ zip (map show [1 .. 9]) [1 .. 9]

find' :: [(String, Int)] -> String -> Maybe (String, Int)
find' mapping haystack = List.find (\(needle, _) -> List.isPrefixOf needle haystack) mapping 

calibration :: [(String, Int)] -> String -> Int
calibration mapping haystack = value $ Maybe.mapMaybe (find' mapping) (List.tails haystack)
  where value values = (snd . head) values * 10 + (snd . last) values

main = do
  input <- getContents

  print $ sum $ map (calibration (Map.toList digits')) (lines input)
  print $ sum $ map (calibration (Map.toList (Map.union digits' words'))) (lines input)

