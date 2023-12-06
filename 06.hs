import Text.Parsec 

type Milliseconds = Int
type Millimeters = Int

sheet :: Parsec String st [(Milliseconds, Millimeters)]
sheet = zip <$> line "Time:" <*> line "Distance:"
  where line prefix = (string prefix) *> spaces *> many ((read <$> many1 digit) <* spaces)

race' :: Milliseconds -> (Millimeters, Milliseconds) -> Int
race' hold (time, distance)
  | hold * (time - hold) > distance = time - hold + 1 - hold 
  | otherwise = race' (hold + 1) (time, distance)

main = do
  input <- getContents

  let races = case parse sheet "parsec" input of
                Left err -> error "Invalid input"
                Right sheet -> sheet

  print $ product $ map (race' 1) races

  let time = (read (foldr (++) "" (map (show . fst) (races))))
  let distance = (read (foldr (++) "" (map (show . snd) (races))))
  print (race' 1 (time, distance))
