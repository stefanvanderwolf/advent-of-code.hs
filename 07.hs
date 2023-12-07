import Text.Parsec 
import Data.List as List (sort, sortBy, group, partition)
import Data.Map as Map (fromList, (!))

type Hand = String
type Bid = Int

hands :: Parsec String st [(Hand, Bid)]
hands = many ((,) <$> (many $ noneOf [' ']) <* space <*> (read <$> many1 digit) <* spaces)

data Rank = HighCard | OnePair | TwoPair | ThreeOfKind | FullHouse | FourOfKind | FiveOfKind
  deriving (Eq, Ord, Show, Read, Bounded, Enum)  

rank :: Hand -> Rank
rank hand 
  | group' == [5] = FiveOfKind
  | group' == [4] = FourOfKind
  | group' == [2, 3] = FullHouse
  | group' == [3] = ThreeOfKind
  | group' == [2, 2] = TwoPair
  | group' == [2] = OnePair
  | otherwise = HighCard
  where group' = sort $ filter (>1) (map length (group (sort hand)))
  
compare' :: String -> Rank -> Hand -> Rank -> Hand -> Ordering
compare' order ar a br b
  | ar == br = compare (map (\c -> values Map.! c) a) (map (\c -> values Map.! c) b)
  | otherwise = compare ar br
  where values = Map.fromList (zip (reverse order) [2..])

upgrade :: Rank -> Int -> Rank
upgrade HighCard 1 = OnePair
upgrade HighCard 2 = ThreeOfKind
upgrade HighCard 3 = FourOfKind
upgrade HighCard 4 = FiveOfKind
upgrade HighCard 5 = FiveOfKind
upgrade OnePair 1  = ThreeOfKind
upgrade OnePair 2  = FourOfKind
upgrade OnePair 3  = FiveOfKind
upgrade TwoPair 1  = FullHouse
upgrade ThreeOfKind 1  = FourOfKind
upgrade ThreeOfKind 2  = FiveOfKind
upgrade FourOfKind 1  = FiveOfKind
upgrade rank _ = rank

upgradeIf :: Hand -> Rank
upgradeIf hand = if (length jokers) > 0 then upgrade (rank rest) (length jokers) else rank hand
  where (jokers, rest) = partition (=='J') hand

main = do
  input <- getContents
  let hands' = case parse hands "parsec" input of
                Left err -> error "Invalid input"
                Right hands -> hands

  let ordered = List.sortBy (\(a, _) (b, _) -> compare' "AKQJT98765432" (rank a) a (rank b) b) hands'
  print $ sum $ map (\(r, bid) -> r * bid) (zip [1..] (map snd ordered))

  let ordered = List.sortBy (\(a, _) (b, _) -> compare' "AKQT98765432J" (upgradeIf a) a (upgradeIf b) b) hands'
  print $ sum $ map (\(r, bid) -> r * bid) (zip [1..] (map snd ordered))

