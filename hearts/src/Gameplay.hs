{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}

module Gameplay where

import Control.Monad (when, unless)

import qualified Control.Monad.Writer (WriterT)
import Control.Monad.Writer as Writer
  
import qualified Control.Monad.State.Lazy as State
import Control.Monad.State.Lazy (State, StateT)

import Control.Monad.Identity (Identity)
import qualified Control.Monad.Identity as Identity

import qualified Data.Sequence as Sequence
import Data.Sequence (Seq, (><))

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set

import Data.Foldable

import qualified Data.IORef as IORef
import Data.IORef (IORef)

import qualified Control.Monad as Monad

import Debug.Trace (trace)

import Cards
import Shuffle
import Game

data EventSourcing monad state event =
  EventSourcing {
    -- FIXME: run in context?
    eventSourcingReadStateM :: monad state,
    eventSourcingProcessEventM :: event -> monad ()
  }

type GameEventSourcing monad = EventSourcing monad GameState GameEvent

-- FIXME: need to nix state at beginning
makeStateEventSourcing :: Monad monad => state -> GameEventSourcing (StateT GameState (WriterT [GameEvent] monad))
makeStateEventSourcing state =
  EventSourcing State.get processGameEventM

makeIOEventSourcing :: (state -> event -> IO state) -> state -> IO (EventSourcing IO state event)
makeIOEventSourcing processEventM' state =
  do stateRef <- IORef.newIORef state
     eventsRef <- IORef.newIORef []
     let readStateM = IORef.readIORef stateRef
     let processEventM event =
           do state <- readStateM
              newState <- processEventM' state event
              IORef.writeIORef stateRef newState
              IORef.modifyIORef eventsRef (\ events -> event : events)
     return (EventSourcing readStateM processEventM)

makeGameIOEventSourcing :: IO (GameEventSourcing IO)
makeGameIOEventSourcing = makeIOEventSourcing (\ state event -> return (processGameEvent state event)) emptyGameState

playerHandM :: Monad monad => GameEventSourcing monad -> PlayerName -> monad Hand
playerHandM eventSourcing player =
  do state <- eventSourcingReadStateM eventSourcing
     return (gameStateHands state Map.! player)

playerStackM :: Monad monad => GameEventSourcing monad -> PlayerName -> monad [Card]
playerStackM ges player =
  do state <- eventSourcingReadStateM ges
     return (gameStateStacks state Map.! player)

type StateWriterEventSourcing state event monad = StateT state (WriterT [event] monad)

processGameEventM :: Monad monad => GameEvent -> StateWriterEventSourcing GameState GameEvent monad ()
processGameEventM event =
  do gameState <- State.get
     State.put (processGameEvent gameState event)
     Writer.tell [event]
     return ()

whoTakesTrickM :: Monad monad => GameEventSourcing monad -> monad (PlayerName, Trick)
whoTakesTrickM ges = do
  state <- eventSourcingReadStateM ges
  let trick = gameStateTrick state
  return (whoTakesTrick trick, trick)

turnOverM :: Monad monad => GameEventSourcing monad -> monad Bool
turnOverM ges = do
  state <- eventSourcingReadStateM ges
  return (turnOver state)

gameOverM :: Monad monad => GameEventSourcing monad -> monad Bool
gameOverM ges = do
  state <- eventSourcingReadStateM ges
  return (gameOver state)

playValidM :: Monad monad => GameEventSourcing monad -> PlayerName -> Card -> monad Bool
playValidM ges playerName card  =
  do state <- eventSourcingReadStateM ges
     return (playValid state playerName card)

currentTrickM :: Monad monad => GameEventSourcing monad -> monad Trick
currentTrickM ges =
  do state <- eventSourcingReadStateM ges
     return (gameStateTrick state)

-- See - error-prone!
processGameCommandM :: Monad monad => GameEventSourcing monad -> GameCommand -> monad [GameEvent]
processGameCommandM ges (DealHands hands) =
  do let event = HandsDealt hands
     eventSourcingProcessEventM ges event
     return [event]
processGameCommandM ges (PlayCard player card) =
  -- FIXME: gameOver event
  do playValid <- playValidM ges player card
     if playValid
      then
        do let event1 = CardPlayed player card
           eventSourcingProcessEventM ges event1
           turnOver <- turnOverM ges
           if turnOver
           then
             do trick <- currentTrickM ges
                let trickTaker = whoTakesTrick trick
                let event2 = TrickTaken trickTaker trick
                eventSourcingProcessEventM ges event2
                let event3 = PlayerTurn trickTaker
                eventSourcingProcessEventM ges event3
                return [event1, event2, event3]
            else
              do nextPlayer <- nextPlayerM ges
                 let event2 = PlayerTurn nextPlayer
                 eventSourcingProcessEventM ges event2
                 return [event1, event2]
      else return []

nextPlayerM :: Monad monad => GameEventSourcing monad -> monad (PlayerName)
nextPlayerM ges =
  do state <- eventSourcingReadStateM ges
     return (nextPlayer state)

gameCommandEventsM :: Monad monad => GameEventSourcing monad -> GameCommand -> monad [GameEvent]
gameCommandEventsM ges gameCommand | trace ("gameCommandsEventsM " ++ show gameCommand) False = undefined
gameCommandEventsM ges gameCommand =
  do gameState <- eventSourcingReadStateM ges
     let (gameState', gameEvents) = processGameCommand gameState gameCommand
     mapM_ (eventSourcingProcessEventM ges) gameEvents
     return (trace ("gameEvents " ++ show gameEvents) gameEvents)

type Strategy monad = PlayerName -> Hand -> Trick -> [Card] -> monad Card

data MonadLifting monad1 monad2  =
  MonadLifting (forall a b . ((b -> monad1 a) -> monad2 (b -> monad2 a)))

-- player with two of clubs at the start leads in the first hand

type EventProcessor monad = GameEvent -> monad [GameCommand]
data Player monad = Player PlayerName (EventProcessor monad)

playerPlay (Player _ play) event = play event
  
data PlayerPackage toMonad =
  forall monad .
    PlayerPackage (Player monad) (MonadLifting monad toMonad)


playerPackageEventProcessorM :: PlayerPackage toMonad -> toMonad (GameEvent -> toMonad [GameCommand])
playerPackageEventProcessorM (PlayerPackage (Player _ play) (MonadLifting lift)) =
  lift play

playerPackageName :: PlayerPackage monad -> PlayerName
playerPackageName (PlayerPackage (Player playerName _) _) = playerName

playerProcessGameEventM :: Monad monad => PlayerName -> GameEvent -> StateT PlayerState monad ()
playerProcessGameEventM playerName event =
  do playerState <- State.get
     let playerState' = playerProcessGameEvent playerName playerState event
     State.put playerState'

strategyPlayer :: Monad monad => PlayerName -> Strategy monad -> Player (StateT PlayerState monad)
strategyPlayer playerName strategy =
  let play event =
        do playerProcessGameEventM playerName event
           playerState <- State.get
           case event of
             HandsDealt hands ->
               if Set.member twoOfClubs (playerHand playerState)
               then return [PlayCard playerName twoOfClubs]
               else return []
             PlayerTurn name' ->
               if playerName == name'
               then do card <- lift (strategy playerName (playerHand playerState) (playerTrick playerState) (playerStack playerState))
                       return [PlayCard playerName card]
               else return []
             CardPlayed name' card -> return []
             TrickTaken name' trick -> return []
  in Player playerName play

type Players monad = Map PlayerName (EventProcessor monad)

playEvent :: Monad monad => Players monad -> GameEvent -> monad (Seq GameCommand)
playEvent players gameEvent | trace ("playEvent " ++ show gameEvent) False = undefined
playEvent players gameEvent =
  Monad.foldM (\ gameCommands playerProcessor ->
                do gameCommands' <- playerProcessor gameEvent
                   return (gameCommands >< Sequence.fromList gameCommands'))
              Sequence.empty
              players

playCommand :: Monad monad => GameEventSourcing monad -> Players monad -> GameCommand -> monad ()
playCommand ges players gameCommand | trace ("playCommand " ++ show gameCommand) False = undefined
playCommand ges players gameCommand =
  do events <- gameCommandEventsM ges gameCommand
     gameOver <- gameOverM ges -- FIXME: should be GameOver in events
     if (trace (show "gameOver " ++ show gameOver) gameOver)
     then return ()
     else
       do gameCommandss <- mapM (playEvent players) (Sequence.fromList events)
          let gameCommands = trace (show "gameCommands " ++ show events) (Monad.join gameCommandss)
          mapM_ (playCommand ges players) gameCommands
          return ()

playGame :: MonadIO monad => GameEventSourcing monad -> Players monad -> [Card] -> monad ()
playGame ges players cards = do
  shuffledCards <- liftIO $ shuffleRounds 10 cards
  let playerNames = Map.keys players
      hands = Map.fromList (zip playerNames (map Set.fromList (distribute (length playerNames) shuffledCards)))
  playCommand ges players (DealHands hands)


-- FIXME: todo State monad with 4-tuple into one


stateToIO :: MonadLifting (StateT state IO) IO
stateToIO =
  MonadLifting (
   \ action ->
     do ioref <- IORef.newIORef undefined
        return (\ x -> do
           state <- IORef.readIORef ioref
           (result, newState) <- State.runStateT (action x) state 
           IORef.writeIORef ioref newState
           return result))

stateToState :: MonadLifting (StateT state1 (State state2)) (State (state1, state2))
stateToState =
  MonadLifting (
   \ action ->
     return (\ x -> do
                (state1, state2) <- State.get
                let ((result, state1'), state2') = State.runState (State.runStateT (action x) state1) state2
                State.put (state1', state2')
                return result))

identityToIO :: MonadLifting Identity IO
identityToIO =
  MonadLifting (
  \ action ->
    return (\ x -> return (Identity.runIdentity (action x))))

-- FIXME unsatisfactory
composeMonadLifting
  :: Monad monad2 =>
     MonadLifting monad1 monad2
     -> MonadLifting monad2 monad3
     -> (b -> monad1 a)
     -> monad2 (monad3 (b -> monad3 a))
composeMonadLifting (MonadLifting lift1) (MonadLifting lift2) =
  let lift3 f = -- b -> monad1 a, need monad3 -N
        do f' <- lift1 f -- monad2 (b -> monad2 a)
           return (lift2 f')
  in lift3

unpackPlayers :: Monad monad => [PlayerPackage monad] -> monad (Players monad)
unpackPlayers playerPackages =
  do eventProcessors <- mapM playerPackageEventProcessorM playerPackages
     let playerNames = map playerPackageName playerPackages
     return (Map.fromList (zip playerNames eventProcessors))

ioPlayerPackage :: PlayerName -> Strategy IO -> PlayerPackage IO
ioPlayerPackage playerName strategy =
  let player = strategyPlayer playerName strategy
  in PlayerPackage player stateToIO



-- strategies

-- |stupid robo player
playAlong :: Strategy IO
playAlong player hand [] stack =
  return (Set.findMin hand)       -- coming up, choose a small first card
playAlong player hand trick stack =
  let (_, firstCard) = last trick
      firstSuit = suit firstCard
      followingCardsOnHand = Set.filter ((== firstSuit) . suit) hand
  in  case Set.lookupMin followingCardsOnHand of
        Nothing ->
          return (Set.findMax hand) -- any card is fine, so try to get rid of high hearts
        Just card ->
          return card           -- otherwise use the minimal following card

-- | interactive player
playInteractive :: Strategy IO
playInteractive player hand trick stack =
  do putStrLn ("Your turn, player " ++ player)
     case trick of
       [] ->
         putStrLn "You lead the next trick."
       _ ->
         putStrLn ("Cards on table: " ++ show (reverse trick))
     let myhand = Set.elems hand
         ncards = Set.size hand
     putStrLn ("Your hand: " ++ pretty myhand)
     putStrLn ("Pick a card (1-" ++ show ncards ++ ")")
     selected <- getNumber (1,ncards)
     return (myhand !! (selected - 1))



gameInteractive :: IO ()
gameInteractive =
  let playerPackages = map (\ playerName -> ioPlayerPackage playerName playInteractive) ["Mike", "Peter", "Annette", "Nicole"]
  in do players <- unpackPlayers playerPackages
        eventSourcing <- makeGameIOEventSourcing
        playGame eventSourcing players deck
