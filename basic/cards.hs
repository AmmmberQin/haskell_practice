data Suit = Spades | Hearts
data Rank = Ten | Jack | Queue | King | Ace
type Card = (Rank, Suit)
type Hand = [Card]
value :: Rank -> Integer
value Ten = 1
value Jack = 2
value Queue = 3
value King = 4
value Ace = 5

cardValue :: Card -> Integer
cardValue (rank, suit) = value rank

backwards :: [a] -> [a]
backwards [] = []
backwards (h:t) = backwards t ++ [h]