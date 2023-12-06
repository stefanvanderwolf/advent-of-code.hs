import Text.Parsec 

import Data.List as List (foldl, concat, minimum)

data Range = Range { destinationStart :: Int, sourceStart :: Int, n :: Int }
  deriving (Show)
data Maps = Maps { source :: String, destination :: String, ranges :: [Range] }
  deriving (Show)
data Almanac = Almanac { seeds :: [Int], maps :: [Maps] }
  deriving (Show)

almanac :: Parsec String st Almanac
almanac = Almanac 
  <$> (string "seeds:" *> (manyTill (space *> decimal) (try newline)))
  <*> (newline *> many mapper)

mapper :: Parsec String st Maps
mapper = Maps 
  <$> (many1 letter <* string "-to-") <*> many1 letter <* (string " map:")
  <*> (manyTill (newline *> range) (try ((eof >> return []) <|> count 2 newline)))
  
range :: Parsec String st Range
range = Range <$> (decimal <* space) <*> (decimal <* space) <*> decimal

decimal = read <$> many1 digit

find'' :: [Range] -> (Int, Int) -> [(Int, Int)]
find'' [] seed = [seed]
find'' all@((Range destinationStart sourceStart n):xs) (seedStart, seedEnd) = 
  if overlapStart < overlapEnd then
    [((overlapStart - sourceStart + destinationStart), (overlapEnd - sourceStart + destinationStart))] 
      ++ (if overlapStart > seedStart then find'' all (seedStart, overlapStart) else [])
      ++ (if seedEnd > overlapEnd then find'' all (overlapEnd, seedEnd) else [])
  else
    find'' xs (seedStart, seedEnd)
  where overlapStart = max seedStart sourceStart
        overlapEnd = min seedEnd (sourceStart + n)

chunked :: [Int] -> [(Int, Int)]
chunked [] = []
chunked (a:b:xs) = (a, a + b):chunked xs

run :: [Maps] -> [(Int, Int)] -> Int
run xs seeds = 
  fst $ minimum $ foldl (\s m -> concat (map (find'' (ranges m)) s)) seeds xs

main = do
  input <- getContents

  print $ case parse almanac "parsec" (input ++ "\n") of
                  Left err -> error "Invalid input"
                  Right almanac ->
                    ((run (maps almanac) (map (\s -> (s, s+1)) (seeds almanac))),
                     (run (maps almanac) ((chunked . seeds) almanac)))
