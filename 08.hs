import Text.Parsec 
import Data.List as List (isSuffixOf)
import Data.Map as Map (Map, fromList, (!), keys)

data Navigation = Navigation { instructions :: String, network :: Map String (String, String) }
  deriving (Show)

navigation :: Parsec String st Navigation
navigation = Navigation 
  <$> (cycle <$> (many $ oneOf "RL")) <* (count 2 newline) <*> (fromList <$> many node)

node :: Parsec String st (String, (String, String))
node =  (,) 
        <$> word <* (space *> char '=' *> space *> char '(') 
        <*> ((,) <$> word <* (string ", ") <*> word <* char ')' <* newline) 
  where word = (many1 (letter <|> digit))

follow :: Map String (String, String) -> (String -> Bool) -> String -> String -> Int
follow network atEnd (instruction:rest) current 
  | (atEnd current) = 0
  | otherwise = 1 + (follow network atEnd rest (move instruction))
  where move 'L' = fst (network ! current)
        move 'R' = snd (network ! current)

main = do
  input <- getContents

  let navigation' = parse navigation "parsec" input 

  print $ fmap (\(Navigation instructions network) -> follow network (=="ZZZ") instructions "AAA") navigation'
  -- LCM only works here because of the input. If there were any offset this
  -- won't work.
  print 
    $ fmap (\(Navigation instructions network) -> foldr lcm 1 (map (follow  network (isSuffixOf "Z") instructions)
      (filter ("A" `isSuffixOf`) (keys network)))) navigation'
