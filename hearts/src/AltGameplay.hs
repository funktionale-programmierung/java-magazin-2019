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

import Debug.Trace (trace, traceShowId, traceIO, traceM)

import Cards
import Game hiding (processGameCommand, processGameEvent, playerProcessGameEvent)
import qualified Shuffle

-- different
type StrategyConstraints m = (HasPlayerState m, MonadIO m)

data PlayerStrategy
  = PlayerStrategy (forall m . StrategyConstraints m => m Card)

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
  -- create game state monad on top of IO
  State.evalStateT (startController players) emptyGameState

type HasGameState m = MonadState GameState m

type GameConstraints m = (MonadIO m, MonadState GameState m, MonadWriter [GameEvent] m)

type ControllerConstraints m = (MonadIO m, MonadState GameState m)

startController :: ControllerConstraints m => [PlayerPackage] -> m ()
startController players = do
  -- setup game state
  let playerNames = map playerName players
  State.modify (\state -> state { statePlayers = playerNames,
                                  stateStacks  = M.fromList (zip playerNames $ repeat [])
                                })
  shuffledCards <- liftIO $ Shuffle.shuffleRounds 10 Cards.deck
  let hands = M.fromList (zip playerNames (map S.fromList (Shuffle.distribute (length playerNames) shuffledCards)))
  gameController players [DealHands hands]

-- state projections

playValidM :: HasGameState m => PlayerName -> Card -> m Bool
playValidM playerName card = do
  state <- State.get
  return (playValid state playerName card)

turnOverM :: HasGameState m => m Bool
turnOverM =
  fmap turnOver State.get

nextPlayerM :: HasGameState m => m PlayerName
nextPlayerM =
  fmap nextPlayer State.get

currentTrickM :: HasGameState m => m Trick
currentTrickM =
  fmap stateTrick State.get

gameOverM :: HasGameState m => m Bool
gameOverM =
  fmap gameOver State.get

runPlayer :: PlayerConstraints m => PlayerCommandProcessor -> GameEvent -> m PlayerCommandProcessor
runPlayer (PlayerCommandProcessor f) gameEvent =
  f gameEvent

announceEvent :: ControllerConstraints m => GameEvent -> m ()
announceEvent (PlayerTurn playerName) =
  liftIO $ putStrLn ("Your turn, " ++ playerName ++ "!")
announceEvent (CardPlayed playerName card) =
  liftIO $ putStrLn (playerName ++ " plays " ++ pretty card)
announceEvent (TrickTaken playerName trick) =
  liftIO $ putStrLn (playerName ++ " takes the trick:\n" ++ pretty (cardsOfTrick trick))
announceEvent (GameOver) = do
  liftIO $ putStrLn "Game Over"
  gameState <- State.get
  let stacks = stateStacks gameState
      playerPenalties = M.map penalty stacks
      loserName = fst $ M.foldrWithKey (\playerName points (loserName, loserPoints) -> 
                                          if points >= loserPoints
                                          then (playerName, points)
                                          else (loserName, loserPoints)
                                       ) ("", 0) playerPenalties
      stackInfo (playerName, points) = do
        putStr playerName
        putStr " has "
        putStr (show points)
        putStrLn " points."
  liftIO $ mapM_ stackInfo (M.assocs playerPenalties)
  liftIO $ putStrLn (loserName ++ " lost the game.")
announceEvent gameEvent =
  liftIO $ putStrLn (show gameEvent)

gameController :: ControllerConstraints m => [PlayerPackage] -> [GameCommand] -> m ()
gameController players commands = do
  -- traceM ("** INCOMING COMMANDS " ++ show commands) 
  (_, events) <- Writer.runWriterT $ mapM_ processGameCommand commands
  mapM_ announceEvent events
  -- st <- State.get
  -- traceM ("** GAMESTATE: " ++ show st)
  -- traceM ("** OUTGOING EVENTS " ++ show events)
  (players', commands') <- Writer.runWriterT $ mapM
    (\pp -> do cp' <- F.foldlM runPlayer (commandProcessor pp) events
               return pp { commandProcessor = cp' }
    ) players
  unless (null commands') $
    gameController players' commands'

processGameCommand :: GameConstraints m => GameCommand -> m ()
processGameCommand command =
  case command of
    DealHands playerHands ->
      processAndPublishEvent (HandsDealt playerHands)
    PlayCard playerName card -> do
      playIsValid <- playValidM playerName card
      if playIsValid then do
          processAndPublishEvent (CardPlayed playerName card)
          turnIsOver <- turnOverM
          if turnIsOver then do
              trick <- currentTrickM
              let trickTaker = whoTakesTrick trick
              processAndPublishEvent (TrickTaken trickTaker trick)
              gameIsOver <- gameOverM
              if gameIsOver 
                then processAndPublishEvent (GameOver)
                else processAndPublishEvent (PlayerTurn trickTaker)
            else do
              nextPlayer <- nextPlayerM
              processAndPublishEvent (PlayerTurn nextPlayer)
        else do
          nextPlayer <- nextPlayerM
          -- processAndPublishEvent (IllegalMove nextPlayer)
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
                           stateHands   = takeCard (stateHands state) playerName card,
                           stateStacks  = stateStacks state,
                           stateTrick   = addToTrick playerName card (stateTrick state)
                         })

processGameEvent (PlayerTurn playerName) = 
  State.modify (\state -> state { statePlayers = rotateTo playerName (statePlayers state) })

processGameEvent (TrickTaken playerName trick) =
  State.modify (\state -> state { stateStacks = (addToStack (stateStacks state) playerName (cardsOfTrick trick)),
                                  stateTrick = emptyTrick
                                })

processGameEvent (GameOver) =
  return ()

--------------------------------------------------------------------------------
-- players

type HasPlayerState m = MonadState PlayerState m

modifyHand  f = State.modify (\playerState -> playerState { playerHand = f (playerHand playerState)})
modifyTrick f = State.modify (\playerState -> playerState { playerTrick = f (playerTrick playerState)})
modifyStack f = State.modify (\playerState -> playerState { playerStack = f (playerStack playerState)})

playerProcessGameEvent :: (HasPlayerState m, PlayerConstraints m) => PlayerName -> GameEvent -> m ()
playerProcessGameEvent playerName gameEvent = do
  case gameEvent of
    HandsDealt hands ->
      State.put (PlayerState { playerHand = hands M.! playerName,
                               playerTrick = emptyTrick,
                               playerStack = [] })

    PlayerTurn turnPlayerName ->
      return ()

    CardPlayed cardPlayerName card -> do
      when (playerName == cardPlayerName) $
        modifyHand (removeCard card)
      modifyTrick (addToTrick cardPlayerName card)

    TrickTaken trickPlayerName trick -> do
      when (playerName == trickPlayerName) $
        modifyStack (cardsOfTrick trick ++)
      modifyTrick (const emptyTrick)

    GameOver ->
      return ()
  -- st <- State.get
  -- traceM ("** AFTER PLAYERPROCESSGAMEEVENT " ++ playerName ++ " " ++ show gameEvent ++ ": " ++ show st)

makePlayerPackage :: PlayerName -> PlayerStrategy -> PlayerPackage
makePlayerPackage playerName strategy =
  PlayerPackage playerName $ strategyPlayer playerName strategy emptyPlayerState

strategyPlayer :: PlayerName -> PlayerStrategy -> PlayerState -> PlayerCommandProcessor
strategyPlayer playerName strategy@(PlayerStrategy chooseCard) playerState =
  PlayerCommandProcessor $ \ event -> do
    (_, nextPlayerState) <- flip State.runStateT playerState $ do
      playerProcessGameEvent playerName event
      playerState <- State.get
      -- traceM ("** PLAYER " ++ playerName ++ ": " ++ show playerState)
      case event of
        HandsDealt _ ->
          when (S.member twoOfClubs (playerHand playerState)) $
            Writer.tell [PlayCard playerName twoOfClubs]

        PlayerTurn turnPlayerName ->
          when (playerName == turnPlayerName) $ do
            -- liftIO $ putStrLn ("Your turn, " ++ playerName)
            card <- chooseCard
            Writer.tell [PlayCard playerName card]

        CardPlayed _ _ ->
          return ()

        TrickTaken _ _ ->
          return ()

        GameOver ->
          return ()

    return (strategyPlayer playerName strategy nextPlayerState)


-- stupid robo player
playAlongStrategy :: PlayerStrategy
playAlongStrategy =
  PlayerStrategy $ do
  playerState <- State.get
  let trick = playerTrick playerState
      hand = playerHand playerState
      firstCard = leadingCardOfTrick trick
      firstSuit = suit firstCard
      followingCardsOnHand = S.filter ((== firstSuit) . suit) hand
  if trickEmpty trick 
    then
      return (S.findMin hand)
    else
      case S.lookupMin followingCardsOnHand of
        Nothing ->
          return (S.findMax hand) -- any card is fine, so try to get rid of high hearts
        Just card ->
          return card           -- otherwise use the minimal following card

-- |interactive player
playInteractive :: PlayerStrategy
playInteractive =
  PlayerStrategy $ do
  playerState <- State.get
  let trick = playerTrick playerState
      hand = playerHand playerState
  if trickEmpty trick
    then
      liftIO $ putStrLn "You lead the next trick!"
    else
      liftIO $ putStrLn ("Cards on table: " ++ show (reverse trick))
  let myhand = S.elems hand
      ncards = S.size hand
  liftIO $ putStrLn ("Your hand:")
  liftIO $ putStrLn (pretty (zip [(1::Integer)..] myhand))
  liftIO $ putStrLn ("Pick a card (1-" ++ show ncards ++ ")")
  selected <- liftIO $ getNumber (1,ncards)
  return (myhand !! (selected - 1))
      
playerMike = makePlayerPackage "Mike" playAlongStrategy
playerPeter = makePlayerPackage "Peter" playInteractive
playerAnnette = makePlayerPackage "Annette" playAlongStrategy
playerNicole = makePlayerPackage "Nicole" playAlongStrategy

start :: IO ()
start = runGame [playerNicole, playerAnnette, playerPeter, playerMike]

{-
remote: 
remote: Create a pull request for 'peters-alt' on GitHub by visiting:        
remote:      https://github.com/funktionale-programmierung/java-magazin-2019/pull/new/peters-alt        
remote: 
-}
