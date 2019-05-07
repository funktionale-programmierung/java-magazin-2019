module Cards where

-- import Test.QuickCheck

data Suit = Spades | Hearts | Diamonds | Clubs
  deriving (Show, Eq)

data Color = Black | Red
  deriving (Show)

-- Define a color function by pattern matching
color :: Suit -> Color
color Spades = Black
color Clubs = Black
color Diamonds = Red
color Hearts = Red

-- |list of all suits
allSuits :: [Suit]
allSuits = [Spades, Hearts, Diamonds, Clubs]

data Rank = Numeric Integer | Jack | Queen | King | Ace
  deriving (Show, Eq, Ord)

-- |rankBeats r1 r2 returns True, if r1 beats r2
rankBeats :: Rank -> Rank -> Bool
rankBeats r1 r2 = r1 > r2

-- |list of all ranks
allRanks :: [Rank]
allRanks = [Numeric i | i <- [2..10]] ++ [Jack, Queen, King, Ace]

-- |playing cards
data Card = Card { rank :: Rank, suit :: Suit }
  deriving (Show, Eq)

cardSuit = suit -- FIXME

-- |cardBeats c1 c2 returns True, if c1 beats c2: they have the same suit and c1's rank is higher
cardBeats :: Card -> Card -> Bool
cardBeats givenCard c = suit givenCard == suit c
                        && rankBeats (rank givenCard)
                                     (rank c)

-- |full deck of all cards
deck :: [Card]
deck = [Card r s | r <- allRanks, s <- allSuits]


-- |during the game, a hand contains at least one card
type Hand = [Card]

-- choose a card from the hand that beats the given card if possible
-- but it does not follow suit!
chooseCard :: Card -> Hand -> Card
chooseCard c [card] = card
chooseCard c (card:hand') =
    if cardBeats card c
    then card
    else chooseCard c hand'


-- the Maybe type
data Maybe' a = Nothing' | Just' a

-- like chooseCard, but follow suit
chooseCard' :: Card -> Hand -> Card
chooseCard' c h =
    chooseCardFollowing c h Nothing

-- | take given card, current hand, and maybe a card of same suit as given card
chooseCardFollowing :: Card -> Hand -> Maybe Card -> Card
chooseCardFollowing c  [card] Nothing = card
chooseCardFollowing c  [card] (Just cardSameSuit) =
    if cardBeats card c
    then card
    else cardSameSuit
chooseCardFollowing c (card:hand') m =
    if cardBeats card c
    then card
    else if suit c == suit card
    then chooseCardFollowing c hand' (Just card)
    else chooseCardFollowing c hand' m

{- alternative way of writing this:
    else chooseCardFollowing c hand
           (if suit c == suit card then Just card else m)
-}

