{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Cards where

-- import Test.QuickCheck
import qualified Control.Monad.Writer (WriterT)
import Control.Monad.Writer as Writer
  
import Control.Monad.State.Lazy as State
import qualified Control.Monad.State.Lazy (State)

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

-- |example cards
aceOfSpades = Card Ace Spades
tenOfHearts = Card (Numeric 10) Hearts
queenOfHearts = Card Queen Hearts
jackOfClub = Card Jack Clubs

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

exampleHand = [jackOfClub, queenOfHearts, tenOfHearts, aceOfSpades]
tenOfClubs = Card (Numeric 10) Clubs

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

-- Games

-- last card is at the front
type Trick = [(Player, Card)]

whoTakesTrick :: Trick -> Player
whoTakesTrick [] = undefined
whoTakesTrick trick =
  let loop player card [] = player
      loop player card ((player', card') : rest) =
        if cardBeats card' card
        then loop player' card' rest
        else loop player card rest
      (player0, card0) : rest = reverse trick
  in loop player0 card0 rest

type Player = String
data GameState = GameState { stateHands :: [(Player, Hand)],
                             stateStacks :: [(Player, [Card])],
                             stateTrick :: Trick }

-- determine whose turn it is
nextPlayer :: GameState -> Maybe Player
nextPlayer state =
  case drop (length (stateTrick state)) (stateHands state) of
    [] -> Nothing
    (player, hand) : rest -> Just player

isHandEmpty :: Hand -> Bool
isHandEmpty [] = True
isHandEmpty _ = False

gameOver :: GameState -> Bool
gameOver state = all (\ (player, hand) -> isHandEmpty hand) (stateHands state)

turnOver :: GameState -> Bool
turnOver state = (length (stateHands state)) == (length (stateTrick state))

data GameEvent =
    HandsDealt [(Player, Hand)]
  | CardPlayed Player Card
  | TrickTaken Player

removeCard :: Hand -> Card -> Hand
removeCard [] _ = undefined
removeCard (c':cs) c
  | c == c' = cs
  | otherwise = c' : removeCard cs c

takeCard :: [(Player, Hand)] -> Player -> Card -> [(Player, Hand)]
takeCard [] _ _ = undefined
takeCard ((player', hand):rest) player card
  | player' == player = (player, removeCard hand card) : rest
  | otherwise         = ((player', hand) : takeCard rest player card)

addToStack :: [(Player, [Card])] -> Player -> [Card] -> [(Player, [Card])]
addToStack [] _ _ = undefined
addToStack ((player', stack):rest) player stack'
  | player' == player = (player', stack' ++ stack):rest
  | otherwise         = (player', stack) : addToStack rest player stack'

processGameEvent :: GameState -> GameEvent -> GameState
processGameEvent state (HandsDealt hands) =
  GameState { stateHands = hands, stateStacks = [], stateTrick = [] }
processGameEvent state (CardPlayed player card) =
  GameState { stateHands = takeCard (stateHands state) player card,
              stateStacks = stateStacks state,
              stateTrick = (player, card) : (stateTrick state) }
processGameEvent state (TrickTaken player) =
  GameState { stateHands = stateHands state,
          stateStacks = addToStack (stateStacks state) player (map snd (stateTrick state)),
          stateTrick = [] }

type EventSourcing state event = StateT state (Writer event)
type MonadEventSourcing monad state event = (MonadState state monad, MonadWriter event monad)

processGameEventM :: MonadEventSourcing monad GameState GameEvent => GameEvent -> monad ()
processGameEventM event =
  do gameState <- State.get
     State.put (processGameEvent gameState event)
     Writer.tell event
     return ()

data GameCommand =
  DealHands [(Player, Hand)]
  | PlayCard Player Card
 
processGameCommand :: GameState -> GameCommand -> (GameState, [GameEvent])
processGameCommand state (DealHands hands) =
  let event = HandsDealt hands
  in (processGameEvent state event, [event])
processGameCommand state (PlayCard player card) =
  let event1 = CardPlayed player card
      state1 = processGameEvent state event1
      trickTaker = whoTakesTrick (stateTrick state1)
      event2 = TrickTaken trickTaker
      state2 = processGameEvent state event2
  in (state2, [event1, event2])

whoTakesTrickM :: MonadState GameState monad => monad Player
whoTakesTrickM = do
  state <- State.get
  return (whoTakesTrick (stateTrick state))

processGameCommandM :: MonadEventSourcing monad GameState GameEvent => GameCommand -> monad ()
processGameCommandM (DealHands hands) = processGameEventM (HandsDealt hands)
processGameCommandM (PlayCard player hands) =
  do processGameEventM (CardPlayed player hands)
     trickTaker <- whoTakesTrickM
     processGameEventM (TrickTaken trickTaker)
     

