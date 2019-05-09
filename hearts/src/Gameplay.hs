{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Gameplay where

import Control.Monad (when)

import qualified Control.Monad.Writer (WriterT)
import Control.Monad.Writer as Writer
  
import Control.Monad.State.Lazy as State
import qualified Control.Monad.State.Lazy (State)

import Cards

-- Games
type Player = String

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

-- |is it legal to play card given the hand and the partial trick on the table
legalCard :: Card -> Hand -> Trick -> Bool
legalCard card hand trick = 
  card `elem` hand &&
  case trick of
    [] -> True -- if trick is empty any card on hand is fine
    _ -> let (_, firstCard) = last trick
             firstSuit = suit firstCard
         in  suit card == firstSuit -- ok if suit is followed
             || all ((/= firstSuit) . suit) hand -- ok if no such suit in hand

data GameState =
  GameState 
  { statePlayers :: [Player],   -- current player at front
    stateHands :: [(Player, Hand)],
    stateStacks :: [(Player, [Card])],
    stateTrick :: Trick
  }

-- |rotate assumes length of input > 0
rotate :: [a] -> [a]
rotate (x : xs) = xs ++ [x]
rotate [] = undefined

-- |rotateTo assumes target exists in input of length > 0
rotateTo :: Eq a => a -> [a] -> [a]
rotateTo y xs@(x : xs') | x == y = xs
                        | otherwise = rotateTo y (xs' ++ [x])
rotateTo y [] = undefined

-- PT: better modeling
-- stateHands :: Data.Map Player Hand
-- stateStacks :: Data.Map Player [Card]
-- ... but it would require an extra mechanism to determine the sequence of players

-- determine whose turn it is
nextPlayer :: GameState -> Player
nextPlayer state =
  head (statePlayers state)

gameOver :: GameState -> Bool
gameOver state = all (\ (player, hand) -> isHandEmpty hand) (stateHands state)

turnOver :: GameState -> Bool
turnOver state = (length (stateHands state)) == (length (stateTrick state))

data GameEvent =
    HandsDealt [(Player, Hand)]
  | CardPlayed Player Card
  | TrickTaken Player

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
  GameState { statePlayers = map fst hands, 
              stateHands = hands,
              stateStacks = [],
              stateTrick = [] }
processGameEvent state (CardPlayed player card) =
  GameState { statePlayers = rotate (statePlayers state),
              stateHands = takeCard (stateHands state) player card,
              stateStacks = stateStacks state,
              stateTrick = (player, card) : (stateTrick state) }
processGameEvent state (TrickTaken player) =
  GameState { statePlayers = rotateTo player (statePlayers state),
              stateHands = stateHands state,
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
  in  if turnOver state1 then
        let trickTaker = whoTakesTrick (stateTrick state1)
            event2 = TrickTaken trickTaker
            state2 = processGameEvent state event2
        in (state2, [event1, event2])
      else (state1, [event1])

whoTakesTrickM :: MonadState GameState monad => monad Player
whoTakesTrickM = do
  state <- State.get
  return (whoTakesTrick (stateTrick state))

turnOverM :: MonadState GameState monad => monad Bool
turnOverM = do
  state <- State.get
  return (turnOver state)

processGameCommandM :: MonadEventSourcing monad GameState GameEvent => GameCommand -> monad ()
processGameCommandM (DealHands hands) = processGameEventM (HandsDealt hands)
processGameCommandM (PlayCard player hands) =
  do processGameEventM (CardPlayed player hands)
     isTurnOver <- turnOverM
     when isTurnOver $ do
       trickTaker <- whoTakesTrickM
       processGameEventM (TrickTaken trickTaker)

