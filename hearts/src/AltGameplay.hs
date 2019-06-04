{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
module AltGameplay where

import qualified Control.Monad.Writer (WriterT)
import Control.Monad.Writer as Writer
  
import qualified Control.Monad.State.Lazy as State
import Control.Monad.State.Lazy (State, StateT)

import Control.Monad as Monad

import qualified Data.Map.Strict as M

import Cards
import Gamedata
import qualified Shuffle

-- same as in Gameplay
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
data PlayerPackage = 
  PlayerPackage
  { playerName :: PlayerName
  , commandProcessor :: (forall m . (MonadIO m, MonadWriter [GameCommand] m) =>
                         GameEvent -> m ())
  }

runGame :: [PlayerPackage] -> IO ()
runGame players = do
  -- create game monad on top of IO
  State.evalStateT (gameController players) emptyGameState

type GameConstraints m = (MonadIO m, MonadState GameState m)

gameController :: GameConstraints m => [PlayerPackage] -> m ()
gameController players = do
  -- setup game state
  let playerNames = map playerName players
  State.modify (\state -> state { statePlayers = playerNames })
  shuffledCards <- liftIO $ Shuffle.shuffleRounds 10 cards
  let hands = M.fromList (zip playerNames (map S.fromList (distribute (length playerNames) shuffledCards)))
  playCommand players (DealHands hands)

playCommand :: [PlayerPackage] -> GameCommand -> GameMonad ()
playCommand players command = undefined

