{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ConstraintKinds #-}

module Gameplay where

import Control.Monad (when, unless)

import qualified Control.Monad.Writer (WriterT)
import Control.Monad.Writer as Writer
  
import Control.Monad.State.Lazy as State
import qualified Control.Monad.State.Lazy (State)

import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Cards

-- Games
type PlayerName = String

-- last card is at the front
type Trick = [(PlayerName, Card)]

whoTakesTrick :: Trick -> PlayerName
whoTakesTrick [] = undefined
whoTakesTrick trick =
  let loop player card [] = player
      loop player card ((player', card') : rest) =
        if cardBeats card' card
        then loop player' card' rest
        else loop player card rest
      (player0, card0) : rest = reverse trick
  in loop player0 card0 rest

-- |is it legal to play card given the hand and the partial trick on the table?
legalCard :: Card -> Hand -> Trick -> Bool
legalCard card hand trick = 
  containsCard card hand &&
  case trick of
    [] -> True -- if trick is empty, then any card on hand is fine
    _ -> let (_, firstCard) = last trick
             firstSuit = suit firstCard
         in  suit card == firstSuit -- ok if suit is followed
             || all ((/= firstSuit) . suit) (S.elems hand) -- ok if no such suit in hand

type PlayerStacks = M.Map PlayerName [Card]
type PlayerHands  = M.Map PlayerName Hand

data GameState =
  GameState 
  { statePlayers :: [PlayerName],   -- current player at front
    stateHands :: PlayerHands,
    stateStacks :: PlayerStacks,
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

-- determine whose turn it is (assumes at least one player)
nextPlayer :: GameState -> PlayerName
nextPlayer state =
  head (statePlayers state)

gameOver :: GameState -> Bool
gameOver state = all isHandEmpty $ M.elems $ stateHands state

turnOver :: GameState -> Bool
turnOver state = M.size (stateHands state) == length (stateTrick state)

data GameEvent =
    HandsDealt (M.Map PlayerName Hand)
  | CardPlayed PlayerName Card
  | TrickTaken PlayerName

takeCard :: PlayerHands -> PlayerName -> Card -> PlayerHands
takeCard playerHand player card =
  M.alter (fmap (removeCard card)) player playerHand

addToStack :: PlayerStacks -> PlayerName -> [Card] -> PlayerStacks
addToStack playerStack player cards =
  M.alter (fmap (cards++)) player playerStack

processGameEvent :: GameState -> GameEvent -> GameState
processGameEvent state (HandsDealt hands) =
  GameState { statePlayers = M.keys hands,
              stateHands = hands,
              stateStacks = M.empty,
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

class Monad monad => MonadEventSourcing monad state event | monad -> state, monad -> event where
  readState    :: monad state
  processEvent :: event -> monad ()

playerHandM :: MonadEventSourcing monad GameState event => PlayerName -> monad Hand
playerHandM player =
  do state <- readState
     return (stateHands state M.! player)

playerStackM :: MonadEventSourcing monad GameState event => PlayerName -> monad [Card]
playerStackM player =
  do state <- readState
     return (stateStacks state M.! player)

trickM :: MonadEventSourcing monad GameState event => monad Trick
trickM =
  do state <- readState
     return (stateTrick state)

type EventSourcing state event monad = StateT state (WriterT [event] monad)

processGameEventM :: Monad monad => GameEvent -> EventSourcing GameState GameEvent monad ()
processGameEventM event =
  do gameState <- State.get
     State.put (processGameEvent gameState event)
     Writer.tell [event]
     return ()

instance Monad monad => MonadEventSourcing (EventSourcing GameState GameEvent monad) GameState GameEvent where
  readState = State.get
  processEvent = processGameEventM

data GameCommand =
  DealHands (M.Map PlayerName Hand)
  | PlayCard PlayerName Card
 
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

whoTakesTrickM :: MonadEventSourcing monad GameState GameEvent => monad PlayerName
whoTakesTrickM = do
  state <- readState
  return (whoTakesTrick (stateTrick state))

turnOverM :: MonadEventSourcing monad GameState GameEvent => monad Bool
turnOverM = do
  state <- readState
  return (turnOver state)

processGameCommandM :: MonadEventSourcing monad GameState GameEvent => GameCommand -> monad ()
processGameCommandM (DealHands hands) = processEvent (HandsDealt hands)
processGameCommandM (PlayCard player card) =
  do processEvent (CardPlayed player card)
     isTurnOver <- turnOverM
     when isTurnOver $ do
       trickTaker <- whoTakesTrickM
       processEvent (TrickTaken trickTaker)

type Strategy monad = PlayerName -> Hand -> Trick -> [Card] -> monad Card

type PlayerStrategies monad = M.Map PlayerName (Strategy monad)

nextPlayerM :: MonadEventSourcing monad GameState GameEvent => monad PlayerName
nextPlayerM =
  do state <- readState
     return (nextPlayer state)

playCard :: Monad monad => PlayerName -> Strategy monad -> EventSourcing GameState GameEvent monad ()
playCard player strategy =
  do hand <- playerHandM player
     trick <- trickM
     stack <- playerStackM player
     card <- lift (lift (strategy player hand trick stack))
     processGameCommandM (PlayCard player card)

playMove :: Monad monad => PlayerStrategies monad -> EventSourcing GameState GameEvent monad ()
playMove strategies =
  do player <- nextPlayerM
     let strategy = strategies M.! player
     -- FIXME: validity check, legalCard
     playCard player strategy

-- returns True if turn is over, False otherwise
playTurn :: Monad monad => PlayerStrategies monad -> EventSourcing GameState GameEvent monad Bool
playTurn strategies =
  do isTurnOver <- turnOverM
     if isTurnOver
     then do playMove strategies
             return True
     else return False

-- strategies

-- |stupid robo player
playAlong :: Monad m => Strategy m
playAlong player hand [] stack =
  return (S.findMin hand)       -- coming up, choose a small first card
playAlong player hand trick stack =
  let (_, firstCard) = last trick
      firstSuit = suit firstCard
      followingCardsOnHand = S.filter ((== firstSuit) . suit) hand
  in  case S.lookupMin followingCardsOnHand of
        Nothing ->
          return (S.findMax hand) -- any card is fine, so try to get rid of high hearts
        Just card ->
          return card           -- otherwise use the minimal following card

-- | interactive player
playInteractive :: MonadIO m => Strategy m
playInteractive player hand trick stack =
  liftIO $ do
  putStrLn ("Your turn, player " ++ player)
  case trick of
    [] ->
      putStrLn "You lead the next trick."
    _ ->
      putStrLn ("Current trick: " ++ show (reverse (map snd trick)))
  let myhand = S.elems hand
      ncards = S.size hand
  putStrLn ("Your hand: " ++ pretty myhand)
  putStrLn ("Pick a card (1-" ++ show ncards ++ ")")
  selected <- getNumber (1,ncards)
  return (myhand !! (selected - 1))


-- |read number in given range from terminal
getNumber :: (Num a, Ord a, Read a, Show a) => (a, a) -> IO a
getNumber (lo, hi) = do
  s <- getLine
  let input = read s
  if lo <= input && input <= hi
  then return input
  else
    do putStrLn ("Input must be between " ++ (show lo) ++ " and " ++ (show hi) ++ ". Try again")
       getNumber (lo, hi)
