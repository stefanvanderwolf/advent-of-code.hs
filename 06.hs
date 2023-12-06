import Text.Parsec 

type Milliseconds = Int
type Millimeters = Int

sheet :: Parsec String st [(Milliseconds, Millimeters)]
sheet = zip <$> line "Time:" <*> line "Distance:"
  where line prefix = (string prefix) *> spaces *> many (decimal <* spaces)
        decimal = read <$> many1 digit

race :: Milliseconds -> Milliseconds -> [(Milliseconds, Millimeters)]
race hold time
  | hold >= time = [] 
  | otherwise = (hold, (time - hold) * hold): race (hold + 1) time 

main = do
  input <- getContents

  let races = case parse sheet "parsec" input of
                Left err -> error "Invalid input"
                Right sheet -> sheet

  print $ product $ map length (map (\(time, distance) -> (filter ((> distance) . snd) (race 0 time))) races)

  let time = (read (foldr (++) "" (map (show . fst) (races))) :: Int)
  let distance = (read (foldr (++) "" (map (show . snd) (races))) :: Int)

  print $ product $ map length (map (\(time, distance) -> (filter ((> distance) . snd) (race 0 time))) [(time, distance)])
