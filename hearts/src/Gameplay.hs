{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}

module Gameplay where

import Control.Monad (when, unless)

import qualified Control.Monad.Writer (WriterT)
import Control.Monad.Writer as Writer
  
import Control.Monad.State.Lazy as State
import qualified Control.Monad.State.Lazy (State, StateT)
import Control.Monad.Trans.Identity (IdentityT)

import qualified Data.Sequence as Sequence
import Data.Sequence (Seq, (><))

import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Data.Foldable

import qualified Control.Monad as Monad

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
  { statePlayers :: [PlayerName],
    stateHands :: PlayerHands,
    stateStacks :: PlayerStacks,
    stateTrick :: Trick
  }
  deriving Show

gameAtBeginning :: GameState -> Bool
gameAtBeginning gameState =
  (null (stateTrick gameState)) && (all null (M.elems (stateHands gameState)))

-- |rotate assumes length of input > 0
rotate :: [a] -> [a]
rotate (x : xs) = xs ++ [x]
rotate [] = undefined

-- |rotateTo assumes target exists in input of length > 0
rotateTo :: Eq a => a -> [a] -> [a]
rotateTo y xs@(x : xs') | x == y = xs
                        | otherwise = rotateTo y (xs' ++ [x])
rotateTo y [] = undefined

computeNextPlayer currentPlayerName playerNames =
  let next [] = head playerNames
      next (playerName:playerNamesRest) =
        if playerName == currentPlayerName
        then head playerNamesRest
        else next playerNamesRest
  in next playerNames
  
-- determine whose turn it is (assumes at least one player)
nextPlayer :: GameState -> PlayerName
nextPlayer state =
  head (statePlayers state)

playValid :: GameState -> PlayerName -> Card -> Bool
playValid gameState playerName card =
  -- FIXME: validate that the player has the card
  if gameAtBeginning gameState
  then card == twoOfClubs
  else nextPlayer gameState == playerName

gameOver :: GameState -> Bool
gameOver state = all isHandEmpty $ M.elems $ stateHands state

turnOver :: GameState -> Bool
turnOver state = M.size (stateHands state) == length (stateTrick state)

data GameEvent =
    HandsDealt (M.Map PlayerName Hand)
  | PlayerTurn PlayerName
  | CardPlayed PlayerName Card
  | TrickTaken PlayerName Trick
  deriving Show

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
processGameEvent state (PlayerTurn player) =
  state { statePlayers = rotateTo player (statePlayers state) }
processGameEvent state (CardPlayed player card) =
  GameState { statePlayers = rotate (statePlayers state),
              stateHands = takeCard (stateHands state) player card,
              stateStacks = stateStacks state,
              stateTrick = (player, card) : (stateTrick state) }
processGameEvent state (TrickTaken player trick) =
  GameState { statePlayers = statePlayers state,
              stateHands = stateHands state,
              stateStacks = addToStack (stateStacks state) player (map snd trick),
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
  deriving Show

processGameCommand :: GameState -> GameCommand -> (GameState, [GameEvent])
processGameCommand state (DealHands hands) =
  let event = HandsDealt hands
  in (processGameEvent state event, [event])
processGameCommand state (PlayCard player card) =
  if playValid state player card
  then   
    let event1 = CardPlayed player card
        state1 = processGameEvent state event1
    in  if turnOver state1 then
          let trick = stateTrick state1
              trickTaker = whoTakesTrick trick
              event2 = TrickTaken trickTaker trick
              state2 = processGameEvent state event2
          in (state2, [event1, event2, PlayerTurn trickTaker])
        else (state1, [event1, PlayerTurn (nextPlayer state1)])
  else (state, [])

whoTakesTrickM :: MonadEventSourcing monad GameState GameEvent => monad (PlayerName, Trick)
whoTakesTrickM = do
  state <- readState
  let trick = stateTrick state
  return (whoTakesTrick trick, trick)

turnOverM :: MonadEventSourcing monad GameState GameEvent => monad Bool
turnOverM = do
  state <- readState
  return (turnOver state)

gameOverM :: MonadEventSourcing monad GameState GameEvent => monad Bool
gameOverM = do
  state <- readState
  return (gameOver state)

playValidM :: MonadEventSourcing monad GameState GameEvent => PlayerName -> Card -> monad Bool
playValidM playerName card  =
  do state <- readState
     return (playValid state playerName card)

currentTrickM :: MonadEventSourcing monad GameState GameEvent => monad Trick
currentTrickM =
  do state <- readState
     return (stateTrick state)

processGameCommandM :: MonadEventSourcing monad GameState GameEvent => GameCommand -> monad [GameEvent]
processGameCommandM (DealHands hands) =
  do let event = HandsDealt hands
     processEvent event
     return [event]
processGameCommandM (PlayCard player card) =
  -- FIXME: gameOver event
  do playValid <- playValidM player card
     if playValid
      then
        do let event1 = CardPlayed player card
           processEvent event1
           turnOver <- turnOverM
           if turnOver
           then
             do trick <- currentTrickM
                let trickTaker = whoTakesTrick trick
                let event2 = TrickTaken trickTaker trick
                processEvent event2
                return [event1, event2, PlayerTurn trickTaker]
            else
              do nextPlayer <- nextPlayerM
                 return [event1, PlayerTurn nextPlayer]
      else return []

gameCommandEventsM :: MonadEventSourcing monad GameState GameEvent => GameCommand -> monad [GameEvent]
gameCommandEventsM gameCommand =
  do gameState <- readState
     let (gameState', gameEvents) = processGameCommand gameState gameCommand
     return gameEvents

class MonadTrans transformer => StrategyT transformer where
  chooseCard :: Monad monad => PlayerName -> Hand -> Trick -> [Card] -> transformer monad Card
  liftStrategy :: Monad monad => monad a -> transformer monad a

type Strategy monad = PlayerName -> Hand -> Trick -> [Card] -> monad Card

-- FIXME: this is how we do it, we want everything to apply
class Monad monad => MonadToIO monad where
  toIO :: monad a -> IO a

class (Monad monad1, Monad monad2) => MonadLifting monad1 monad2 where
  liftMonad :: monad1 a -> monad2 a

instance MonadLifting monad1 monad2 => MonadLifting monad1 (StateT state monad2) where
  liftMonad = lift . liftMonad

data StrategyPackage toMonad =
  forall monad  . (Monad monad, MonadLifting monad toMonad) =>
    StrategyPackage (Strategy monad)

packageStrategy :: StrategyPackage toMonad -> Strategy toMonad
packageStrategy (StrategyPackage strategy) =
  \ playerName hand trick stack ->
    liftMonad (strategy playerName hand trick stack)

-- player with two of clubs at the start leads in the first hand

type Player monad = GameEvent -> monad [GameCommand]

data PlayerPackage toMonad =
  forall monad . MonadLifting monad toMonad =>
    PlayerPackage (Player monad)

packagePlayer :: PlayerPackage monad -> Player monad
packagePlayer (PlayerPackage player) =
  \ event -> liftMonad (player event)

data PlayerState =
  PlayerState { playerName :: PlayerName,
                playerHand :: Hand,
                playerTrick :: Trick,
                playerStack :: [Card] }
  deriving Show

playerProcessGameEvent :: PlayerState -> GameEvent -> PlayerState
playerProcessGameEvent state (HandsDealt hands) =
  PlayerState { playerName = playerName state,
                playerHand = hands M.! (playerName state),
                playerTrick = [],
                playerStack = [] }
playerProcessGameEvent state (CardPlayed player card)
  | player == playerName state =
    state { playerHand = removeCard card (playerHand state),
            playerTrick = (player, card) : (playerTrick state) }
  | otherwise = state
playerProcessGameEvent state (TrickTaken player trick)
  | player == playerName state =
    state { playerTrick = [],
            playerStack = (map snd trick) ++ (playerStack state) }
  | otherwise = state

playerProcessGameEventM :: Monad monad => GameEvent -> StateT PlayerState monad ()
playerProcessGameEventM event =
  do playerState <- State.get
     let playerState' = playerProcessGameEvent playerState event
     State.put playerState'

twoOfClubs = Card Clubs (Numeric 2)

strategyPlayer :: Monad monad => Strategy monad -> GameEvent -> StateT PlayerState monad [GameCommand]
strategyPlayer strategy event =
  do playerProcessGameEventM event
     playerState <- State.get
     let name = playerName playerState
     case event of
       HandsDealt hands ->
         if S.member twoOfClubs (playerHand playerState)
         then return [PlayCard name twoOfClubs]
         else return []
       PlayerTurn name' ->
         if name == name'
         then do card <- lift (strategy name (playerHand playerState) (playerTrick playerState) (playerStack playerState))
                 return [PlayCard name card]
         else return []
       CardPlayed name' card -> return []
       TrickTaken name' trick -> return []

type Players monad = M.Map PlayerName (PlayerPackage monad)

playEvent :: Monad monad => Players monad -> GameEvent -> monad (Seq GameCommand)
playEvent players gameEvent =
  Monad.foldM (\ gameCommands playerPackage ->
                do gameCommands' <- packagePlayer playerPackage gameEvent
                   return (gameCommands >< Sequence.fromList gameCommands'))
              Sequence.empty
              players

playCommand :: MonadEventSourcing monad GameState GameEvent => Players monad -> GameCommand -> monad ()
playCommand players gameCommand =
  do events <- gameCommandEventsM gameCommand
     gameOver <- gameOverM -- FIXME: should be GameOver in events
     if gameOver
     then return ()
     else
       do gameCommandss <- mapM (playEvent players) (Sequence.fromList events)
          let gameCommands = Monad.join gameCommandss
          mapM_ (playCommand players) gameCommands
          return ()

playGame :: MonadEventSourcing monad GameState GameEvent => Players monad -> PlayerHands -> monad ()
playGame players hands = playCommand players (DealHands hands)

-- |distribute n-ways
distribute :: Int -> [a] -> [[a]]
distribute m xs = [ extract (drop i xs) | i <- [0 .. m-1]]
  where
    extract [] = []
    extract (x:xs) = x : extract (drop (m-1) xs)

nextPlayerM :: MonadEventSourcing monad GameState GameEvent => monad (PlayerName)
nextPlayerM =
  do state <- readState
     return (nextPlayer state)

playCard' :: Monad monad => PlayerName -> Strategy monad -> EventSourcing GameState GameEvent monad ()
playCard' player strategy =
  do hand <- playerHandM player
     trick <- trickM
     stack <- playerStackM player
     card <- lift (lift (strategy player hand trick stack))
     processGameCommandM (PlayCard player card)
     return ()

type PlayerStrategies monad = M.Map PlayerName (StrategyPackage monad)

playMove' :: Monad monad => PlayerStrategies monad -> EventSourcing GameState GameEvent monad ()
playMove' strategies =
  do player <- nextPlayerM
     let strategyPackage = strategies M.! player
     -- FIXME: validity check, legalCard
     playCard' player (packageStrategy strategyPackage)

-- returns True if turn is over, False otherwise
playTurn' :: Monad monad => PlayerStrategies monad -> EventSourcing GameState GameEvent monad Bool
playTurn' strategies =
  do isTurnOver <- turnOverM
     if isTurnOver
     then do playMove' strategies
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
