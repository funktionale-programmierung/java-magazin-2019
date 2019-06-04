{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
module AltGameplay where

import qualified Control.Monad.Writer (WriterT)
import Control.Monad.Writer as Writer
  
import Control.Monad.State.Lazy as State
import Control.Monad.State.Lazy (State, StateT)

import Control.Monad as Monad

import qualified Data.Foldable as F
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Cards
import Gamedata
import qualified Shuffle

-- from Gameplay

takeCard :: PlayerHands -> PlayerName -> Card -> PlayerHands
takeCard playerHand playerName card =
  M.alter (fmap (removeCard card)) playerName playerHand

addToStack :: PlayerStacks -> PlayerName -> [Card] -> PlayerStacks
addToStack playerStack player cards =
  M.alter (fmap (cards++)) player playerStack

data GameCommand =
    DealHands (M.Map PlayerName Hand)
  | PlayCard PlayerName Card
  deriving Show

data GameEvent =
    HandsDealt (M.Map PlayerName Hand)
  | PlayerTurn PlayerName
  | CardPlayed PlayerName Card
  | TrickTaken PlayerName Trick
  deriving Show

-- different
type PlayerConstraints m = (MonadIO m, MonadWriter [GameCommand] m)

data PlayerCommandProcessor =
  PlayerCommandProcessor (forall m . PlayerConstraints m =>
                         GameEvent -> m PlayerCommandProcessor)

data PlayerPackage = 
  PlayerPackage
  { playerName :: PlayerName
  , commandProcessor :: PlayerCommandProcessor
  }

-- main entry point
runGame :: [PlayerPackage] -> IO ()
runGame players = do
  -- create game monad on top of IO
  fmap fst $ State.evalStateT (Writer.runWriterT $ gameController players) emptyGameState

type GameConstraints m = (MonadIO m, MonadState GameState m, MonadWriter [GameEvent] m)

gameController :: GameConstraints m => [PlayerPackage] -> m ()
gameController players = do
  -- setup game state
  let playerNames = map playerName players
  State.modify (\state -> state { statePlayers = playerNames })
  shuffledCards <- liftIO $ Shuffle.shuffleRounds 10 Cards.deck
  let hands = M.fromList (zip playerNames (map S.fromList (Shuffle.distribute (length playerNames) shuffledCards)))
  playCommands players [DealHands hands]

playValidM :: GameConstraints m => PlayerName -> Card -> m Bool
playValidM playerName card = do
  state <- State.get
  return $ playValid state playerName card

turnOverM :: GameConstraints m => m Bool
turnOverM =
  fmap turnOver State.get

nextPlayerM :: GameConstraints m => m PlayerName
nextPlayerM =
  fmap nextPlayer State.get

currentTrickM :: GameConstraints m => m Trick
currentTrickM =
  fmap stateTrick State.get


runPlayer :: PlayerConstraints m => GameEvent -> PlayerCommandProcessor -> m PlayerCommandProcessor
runPlayer gameEvent (PlayerCommandProcessor f) =
  f gameEvent


playCommands :: GameConstraints m => [PlayerPackage] -> [GameCommand] -> m ()
playCommands players commands = do
  (_, events) <- listen $ mapM_ processGameCommand commands
  (players', commands') <- Writer.runWriterT $ mapM
    (\pp -> do cp' <- F.foldrM runPlayer (commandProcessor pp) events
               return pp { commandProcessor = cp' }
    ) players
  playCommands players' commands'

processGameCommand :: GameConstraints m => GameCommand -> m ()
processGameCommand command =
  case command of
    DealHands playerHands ->
      processAndPublishEvent (HandsDealt playerHands)
    PlayCard playerName card -> do
      playIsValid <- playValidM playerName card
      when playIsValid $ do
        processAndPublishEvent (CardPlayed playerName card)
        turnIsOver <- turnOverM
        if turnIsOver then do
            trick <- currentTrickM
            let trickTaker = whoTakesTrick trick
            processAndPublishEvent (TrickTaken trickTaker trick)
            processAndPublishEvent (PlayerTurn trickTaker)
          else do
            nextPlayer <- nextPlayerM
            processAndPublishEvent (PlayerTurn nextPlayer)

processAndPublishEvent :: GameConstraints m => GameEvent -> m ()
processAndPublishEvent gameEvent = do
  processGameEvent gameEvent
  Writer.tell [gameEvent]

processGameEvent :: GameConstraints m => GameEvent -> m ()
processGameEvent (HandsDealt playerHands) =
  State.modify (\state -> state { stateHands = playerHands })
  -- no good to broadcast this event as everybody knows everybody else's hand

processGameEvent (CardPlayed playerName card) =
  State.modify (\state ->
                   state { statePlayers = rotate (rotateTo playerName (statePlayers state)),
                           stateHands = takeCard (stateHands state) playerName card,
                           stateStacks = stateStacks state,
                           stateTrick = (playerName, card) : (stateTrick state)
                         })

processGameEvent (PlayerTurn playerName) = 
  State.modify (\state -> state { statePlayers = rotateTo playerName (statePlayers state) })

processGameEvent (TrickTaken playerName trick) =
  State.modify (\state -> state { stateStacks = (addToStack (stateStacks state) playerName (cardsOfTrick trick)),
                                  stateTrick = []
                                })
