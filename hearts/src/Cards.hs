module Cards where

import qualified Data.Set as Set
import Data.Set (Set)

data Suit = Diamonds | Clubs | Spades | Hearts
  deriving (Show, Eq, Ord)

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
data Card = Card { suit :: Suit, rank :: Rank }
  deriving (Show, Eq, Ord)

-- |cardBeats c1 c2 returns True, if c1 beats c2: they have the same suit and c1's rank is higher
cardBeats :: Card -> Card -> Bool
cardBeats givenCard c = suit givenCard == suit c
                        && rankBeats (rank givenCard)
                                     (rank c)

-- |full deck of all cards
deck :: [Card]
deck = [Card suit rank | rank <- allRanks, suit <- allSuits]


-- |during the game, a hand contains at least one card
type Hand = Set Card

isHandEmpty :: Hand -> Bool
isHandEmpty hand = Set.null hand

removeCard :: Card -> Hand -> Hand
removeCard card hand = Set.delete card hand

containsCard :: Card -> Hand -> Bool
containsCard card hand = Set.member card hand

{-
-- choose a card from the hand that beats the given card if possible
-- but it does not follow suit!
chooseCard :: Card -> Hand -> Card
chooseCard c [card] = card
chooseCard c (card:hand') =
    if cardBeats card c
    then card
    else chooseCard c hand'

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
-}

class Penalty a where
  penalty :: a -> Int

instance Penalty Card where
  penalty (Card Hearts r) = 1
  penalty (Card Spades Queen) = 13
  penalty (Card _ _) = 0

instance Penalty a => Penalty [a] where
  penalty xs = sum (map penalty xs)

class Pretty a where
  pretty :: a -> String

instance Pretty Rank where
  pretty (Numeric i) = show i
  pretty r = show r

instance Pretty Suit where
  pretty s = show s

instance Pretty Card where
  pretty c = pretty (rank c) ++ " of " ++ pretty (suit c)

instance Pretty a => Pretty [a] where
  pretty [] = ""
  pretty [x] = pretty x
  pretty (x:xs) = pretty x ++ " and " ++ pretty xs

