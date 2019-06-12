{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}

module GameplayT where

import qualified Control.Monad.State.Lazy as State
import Control.Monad.State.Lazy (State, StateT, MonadState)
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

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)

import Debug.Trace (trace)


class MonadTrans monadT => Player player monadT event command | player -> monadT, player -> event, player -> command where
  play :: Monad monad => player -> EventProcessor (monadT monad) event command

type EventProcessor monad event command = event -> monad [command]

type Players monad event command = Map PlayerName (EventProcessor monad event command)

emptyPlayers :: Players monad event command
emptyPlayers = Map.empty

addPlayer :: (Monad monad, Player player monadT event command) => Players monad event command -> PlayerName -> player -> Players (monadT monad) event command
addPlayer players playerName player =
  let liftProcessor processor = \ event -> lift (processor event)
  in Map.insert playerName (play player) (Map.map liftProcessor players)

class MonadTrans monadT => Strategy strategy monadT | strategy -> monadT  where
  chooseCard :: Monad monad => strategy -> PlayerName -> PlayerState -> (monadT monad) Card

playerProcessGameEventM :: MonadState PlayerState monad => PlayerName -> GameEvent -> monad ()
playerProcessGameEventM playerName event =
  do playerState <- State.get
     let playerState' = playerProcessGameEvent playerName event playerState
     State.put playerState'

strategyPlay
  :: (MonadState PlayerState (monadT monad),
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

type EventSourcingT state event monad = StateT state (WriterT [event] monad)
type MonadEventSourcing state event monad =
  (MonadState state monad, MonadWriter [event] monad)

runEventSourcing :: (forall monad . MonadEventSourcing state event monad => monad ()) -> state -> (state, [event])
runEventSourcing action state =
  Writer.runWriter (State.execStateT action state)

type MonadGameEventSourcing monad = MonadEventSourcing GameState GameEvent monad
type GameEventSourcingT monad = EventSourcingT GameState GameEvent monad

eventSourcingReadStateM :: MonadEventSourcing state event monad => monad state
eventSourcingReadStateM = State.get

playerHandM :: MonadGameEventSourcing monad => PlayerName -> monad Hand
playerHandM playerName =
  do state <- eventSourcingReadStateM
     return (gameStateHands state Map.! playerName)

playerStackM :: MonadGameEventSourcing monad => PlayerName -> monad (Set Card)
playerStackM player =
  do state <- eventSourcingReadStateM
     return (gameStateStacks state Map.! player)

trickM :: MonadGameEventSourcing monad => monad Trick
trickM =
  do state <- eventSourcingReadStateM
     return (gameStateTrick state)

processGameEventM :: MonadGameEventSourcing monad => GameEvent -> monad ()
processGameEventM event =
  do gameState <- State.get
     State.put (processGameEvent event gameState)
     Writer.tell [event]
     return ()

whoTakesTrickM :: MonadGameEventSourcing monad => monad (PlayerName, Trick)
whoTakesTrickM = do
  state <- eventSourcingReadStateM
  let trick = gameStateTrick state
  return (whoTakesTrick trick, trick)

turnOverM :: MonadGameEventSourcing monad => monad Bool
turnOverM = do
  state <- eventSourcingReadStateM
  return (turnOver state)

gameOverM :: MonadGameEventSourcing monad => monad Bool
gameOverM = do
  state <- eventSourcingReadStateM
  return (gameOver state)

playValidM :: MonadGameEventSourcing monad => PlayerName -> Card -> monad Bool
playValidM playerName card  =
  do state <- eventSourcingReadStateM
     return (playValid state playerName card)

currentTrickM :: MonadGameEventSourcing monad => monad Trick
currentTrickM =
  do state <- eventSourcingReadStateM
     return (gameStateTrick state)

nextPlayerM :: MonadGameEventSourcing monad => monad (PlayerName)
nextPlayerM =
  do state <- eventSourcingReadStateM
     return (nextPlayer state)

processGameCommandM :: MonadGameEventSourcing monad => GameCommand -> monad ()
processGameCommandM (DealHands playerHands) =
   processGameEventM (HandsDealt playerHands)
processGameCommandM (PlayCard playerName card) =
   do playIsValid <- playValidM playerName card
      if playIsValid then
        do processGameEventM (CardPlayed playerName card)
           turnIsOver <- turnOverM
           if turnIsOver then
             do trick <- currentTrickM
                let trickTaker = whoTakesTrick trick
                processGameEventM (TrickTaken trickTaker trick)
                gameIsOver <- gameOverM
                if gameIsOver 
                then processGameEventM (GameOver)
                else processGameEventM (PlayerTurn trickTaker)
           else
             do nextPlayer <- nextPlayerM
                processGameEventM (PlayerTurn nextPlayer)
      else
        do nextPlayer <- nextPlayerM
           processGameEventM (IllegalMove nextPlayer)
           processGameEventM (PlayerTurn nextPlayer)

gameCommandEventsM :: MonadGameEventSourcing monad => GameCommand -> monad [GameEvent]
gameCommandEventsM gameCommand | trace ("gameCommandsEventsM " ++ show gameCommand) False = undefined
gameCommandEventsM gameCommand =
  do gameState <- eventSourcingReadStateM
     let (gameState', gameEvents) = runEventSourcing (processGameCommandM gameCommand) gameState
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
  let playerNames = Map.keys players
      hands = Map.fromList (zip playerNames (map Set.fromList (distribute (length playerNames) shuffledCards)))
  playCommand players (DealHands hands)

-- |stupid robo player

data PlayAlong

instance Strategy PlayAlong IdentityT where
-- playAlong ::  Monad monad => PlayerState -> IdentityT monad Card
  chooseCard _ _ playerState =
    case playerTrick playerState of
      [] -> return (Set.findMin (playerHand playerState))       -- coming up, choose a small first card
      trick ->
        let hand = playerHand playerState
            (_, firstCard) = last trick
            firstSuit = suit firstCard
            followingCardsOnHand = Set.filter ((== firstSuit) . suit) hand
        in  case Set.lookupMin followingCardsOnHand of
              Nothing ->
                return (Set.findMax hand) -- any card is fine, so try to get rid of high hearts
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
           myhand = Set.elems hand
           ncards = Set.size hand
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

