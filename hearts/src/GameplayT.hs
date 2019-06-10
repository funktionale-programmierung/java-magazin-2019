{-# LANGUAGE TupleSections #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE InstanceSigs #-}

module GameplayT where

import qualified Control.Monad.State.Lazy as State
import Control.Monad.State.Lazy (State, StateT)
import Control.Monad.Trans.Class

import qualified Control.Monad.Writer (WriterT)
import Control.Monad.Writer as Writer

import Control.Monad.Identity (Identity, IdentityT)
import qualified Control.Monad.Identity as Identity

import qualified Data.Sequence as Sequence
import Data.Sequence (Seq, (><))

import qualified Control.Monad as Monad

import Control.Monad.Trans.Free

import qualified Data.IORef as IORef
import Data.IORef (IORef)

import Cards
import Shuffle
import Game
import RWT

import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Debug.Trace (trace)


class MonadTrans monadT => Player player monadT event command | player -> monadT, player -> event, player -> command where
  play :: Monad monad => player -> event -> (monadT monad) [command]

class Monad monad => Player' player monad event command | player -> monad, player -> event, player -> command where
  play' :: player -> event ->  monad [command]

type EventProcessor monad event command = event -> monad [command]

type Players monad event command = M.Map PlayerName (EventProcessor monad event command)

emptyPlayers :: Players monad event command
emptyPlayers = M.empty

addPlayer :: (Monad monad, Player player monadT event command) => Players monad event command -> PlayerName -> player -> Players (monadT monad) event command
addPlayer players playerName player =
  let liftProcessor processor = \ event -> lift (processor event)
  in M.insert playerName (play player) (M.map liftProcessor players)

addPlayer' :: Player' player monad event command =>  Players monad event command -> PlayerName -> player -> Players monad event command
addPlayer' players playerName player' =
  M.insert playerName (play' player') players
  
class MonadTrans monadT => Strategy strategy monadT | strategy -> monadT  where
  chooseCard :: Monad monad => strategy -> PlayerName -> PlayerState -> (monadT monad) Card

class Monad monad => Strategy' strategy monad | strategy -> monad  where
  chooseCard' :: Monad monad => strategy -> PlayerName -> PlayerState -> monad Card

playerProcessGameEventM :: State.MonadState PlayerState monad => PlayerName -> GameEvent -> monad ()
playerProcessGameEventM playerName event =
  do playerState <- State.get
     let playerState' = playerProcessGameEvent playerName playerState event
     State.put playerState'

-- FIXME: any point to this - i.e. any point not shortening monadT monad -> monad
strategyPlay
  :: (State.MonadState PlayerState (monadT monad),
      Strategy strategy monadT, Monad monad,
      Monad (monadT monad)) =>
     strategy -> PlayerName -> GameEvent -> monadT monad [GameCommand]
strategyPlay strategy playerName event =
  do playerProcessGameEventM playerName event
     playerState <- State.get
     case event of
       HandsDealt hands ->
         if containsCard twoOfClubs (playerHand playerState)
         then return [PlayCard playerName twoOfClubs]
         else return []
       PlayerTurn name' ->
         if playerName == name'
         then do card <- chooseCard strategy playerName playerState
                 return [PlayCard playerName card]
         else return []
       CardPlayed name' card -> return []
       TrickTaken name' trick -> return []

strategyPlay'
  :: (State.MonadState PlayerState m, Strategy' strategy m) =>
     strategy -> PlayerName -> GameEvent -> m [GameCommand]
strategyPlay' strategy' playerName event =
  do playerProcessGameEventM playerName event
     playerState <- State.get
     case event of
       HandsDealt hands ->
         if containsCard twoOfClubs (playerHand playerState)
         then return [PlayCard playerName twoOfClubs]
         else return []
       PlayerTurn name' ->
         if playerName == name'
         then do card <- chooseCard' strategy' playerName playerState
                 return [PlayCard playerName card]
         else return []
       CardPlayed name' card -> return []
       TrickTaken name' trick -> return []


{-
do playerState <- State.get
      card <- chooseCard' strategy' playerName playerState
      return [PlayCard playerName card]
-}

type EventSourcingT state event monad = StateT state (WriterT [event] monad)

type GameEventSourcingT monad = EventSourcingT GameState GameEvent monad

eventSourcingReadStateM :: Monad monad => EventSourcingT state event monad state
eventSourcingReadStateM = State.get

playerHandM :: Monad monad => PlayerName -> GameEventSourcingT monad Hand
playerHandM playerName =
  do state <- eventSourcingReadStateM
     return (gameStateHands state M.! playerName)

playerStackM :: Monad monad => PlayerName -> GameEventSourcingT monad [Card]
playerStackM player =
  do state <- eventSourcingReadStateM
     return (gameStateStacks state M.! player)

trickM :: Monad monad => GameEventSourcingT monad Trick
trickM =
  do state <- eventSourcingReadStateM
     return (gameStateTrick state)

processGameEventM :: Monad monad => GameEvent -> GameEventSourcingT monad ()
processGameEventM event =
  do gameState <- State.get
     State.put (processGameEvent gameState event)
     Writer.tell [event]
     return ()

whoTakesTrickM :: Monad monad => GameEventSourcingT monad (PlayerName, Trick)
whoTakesTrickM = do
  state <- eventSourcingReadStateM
  let trick = gameStateTrick state
  return (whoTakesTrick trick, trick)

turnOverM :: Monad monad => GameEventSourcingT monad Bool
turnOverM = do
  state <- eventSourcingReadStateM
  return (turnOver state)

gameOverM :: Monad monad => GameEventSourcingT monad Bool
gameOverM = do
  state <- eventSourcingReadStateM
  return (gameOver state)

playValidM :: Monad monad => PlayerName -> Card -> GameEventSourcingT monad Bool
playValidM playerName card  =
  do state <- eventSourcingReadStateM
     return (playValid state playerName card)

currentTrickM :: Monad monad => GameEventSourcingT monad Trick
currentTrickM =
  do state <- eventSourcingReadStateM
     return (gameStateTrick state)

nextPlayerM :: Monad monad => GameEventSourcingT monad (PlayerName)
nextPlayerM =
  do state <- eventSourcingReadStateM
     return (nextPlayer state)

gameCommandEventsM :: Monad monad => GameCommand -> GameEventSourcingT monad [GameEvent]
gameCommandEventsM gameCommand | trace ("gameCommandsEventsM " ++ show gameCommand) False = undefined
gameCommandEventsM gameCommand =
  do gameState <- eventSourcingReadStateM
     let (gameState', gameEvents) = processGameCommand gameState gameCommand
     mapM_ processGameEventM gameEvents
     return (trace ("gameEvents " ++ show gameEvents) gameEvents)

playEvent :: Monad monad => Players monad GameEvent GameCommand -> GameEvent -> monad (Seq GameCommand)
playEvent players gameEvent | trace ("playEvent " ++ show gameEvent) False = undefined
playEvent players gameEvent =
  Monad.foldM (\ gameCommands playerProcessor ->
                do gameCommands' <- playerProcessor gameEvent
                   return (gameCommands >< Sequence.fromList gameCommands'))
              Sequence.empty
              players

playCommand :: Monad monad => Players monad GameEvent GameCommand -> GameCommand -> GameEventSourcingT monad ()
playCommand players gameCommand | trace ("playCommand " ++ show gameCommand) False = undefined
playCommand players gameCommand =
  do events <- gameCommandEventsM gameCommand
     gameOver <- gameOverM -- FIXME: should be GameOver in events
     if (trace (show "gameOver " ++ show gameOver) gameOver)
     then return ()
     else
       -- FIXME: do playEvent in GameEventSourcingT?
       do gameCommandss <- mapM (\ gameEvent -> lift (lift (playEvent players gameEvent))) (Sequence.fromList events)
          let gameCommands = trace (show "gameCommands " ++ show events) (Monad.join gameCommandss)
          mapM_ (playCommand players) gameCommands
          return ()

playGame :: Monad monad => Players monad GameEvent GameCommand -> [Card] -> GameEventSourcingT monad ()
playGame players cards = do
  -- shuffledCards <- liftIO $ shuffleRounds 10 cards
  let shuffledCards = cards
  -- FIXME: split here
  let playerNames = M.keys players
      hands = M.fromList (zip playerNames (map S.fromList (distribute (length playerNames) shuffledCards)))
  playCommand players (DealHands hands)

-- |stupid robo player

data PlayAlong

instance Strategy PlayAlong IdentityT where
-- playAlong ::  Monad monad => PlayerState -> IdentityT monad Card
  chooseCard _ _ playerState =
    case playerTrick playerState of
      [] -> return (S.findMin (playerHand playerState))       -- coming up, choose a small first card
      trick ->
        let hand = playerHand playerState
            (_, firstCard) = last trick
            firstSuit = suit firstCard
            followingCardsOnHand = S.filter ((== firstSuit) . suit) hand
        in  case S.lookupMin followingCardsOnHand of
              Nothing ->
                return (S.findMax hand) -- any card is fine, so try to get rid of high hearts
              Just card ->
                return card           -- otherwise use the minimal following card



-- | interactive player
data PlayInteractive = PlayInteractive PlayerName

-- oh no: only 1 state for all players
instance Strategy PlayInteractive (RWT PlayerState) where
  chooseCard _ playerName playerState =
    do RWT.putString ("Your turn, player " ++ playerName)
       case playerTrick playerState of
         [] ->
           RWT.putString "You lead the next trick."
         trick ->
           RWT.putString ("Cards on table: " ++ show (reverse trick))
       let hand = playerHand playerState
           myhand = S.elems hand
           ncards = S.size hand
       RWT.putString ("Your hand: " ++ pretty myhand)
       RWT.putString ("Pick a card (1-" ++ show ncards ++ ")")
       selected <- RWT.getNumber (1,ncards)
       return (myhand !! (selected - 1))

instance Player PlayInteractive (RWT PlayerState) GameEvent GameCommand where
   play (player@(PlayInteractive playerName)) event = strategyPlay player playerName event

gameInteractive :: IO ()
gameInteractive =
  do let players1 = addPlayer (emptyPlayers :: Players Identity GameEvent GameCommand) "Mike" (PlayInteractive "Mike")
         players2 = addPlayer players1 "Peter" (PlayInteractive "Peter")
         players3 = addPlayer players2 "Nicole" (PlayInteractive "Nicole")
         players4 = addPlayer players3 "Annette" (PlayInteractive "Annette")
     state1 <- IORef.newIORef emptyPlayerState
     state2 <- IORef.newIORef emptyPlayerState
     state3 <- IORef.newIORef emptyPlayerState
     state4 <- IORef.newIORef emptyPlayerState
     let rwt = runWriterT (State.evalStateT (playGame players4 deck) emptyGameState)
     ((), events) <- runRWTIO (runRWTIO (runRWTIO (runRWTIO (return . Identity.runIdentity) state1) state2) state3) state4 rwt
     return ()

