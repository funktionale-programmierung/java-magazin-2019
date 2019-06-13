{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
module AltGameplay where

import Control.Conditional (ifM)
import qualified Control.Monad.Writer (WriterT)
import Control.Monad.Writer as Writer
  
import Control.Monad.State.Lazy as State
import Control.Monad.State.Lazy (State, StateT)

import Control.Monad as Monad

import qualified Data.Foldable as Foldable
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map, (!))
import qualified Data.Set as Set

import Debug.Trace (trace, traceShowId, traceIO, traceM)

import Cards
import Game hiding (processGameCommandM, processGameEvent, playerProcessGameEvent)
import qualified Shuffle

-- different
type StrategyInterface m = (HasPlayerState m, MonadIO m)

data PlayerStrategy
  = PlayerStrategy { chooseCard :: forall m . StrategyInterface m => m Card }

type PlayerInterface m = (MonadIO m, MonadWriter [GameCommand] m)

data PlayerEventProcessor =
  PlayerEventProcessor (forall m . PlayerInterface m =>
                         GameEvent -> m PlayerEventProcessor)

data Player = 
  Player
  { playerName :: PlayerName
  , eventProcessor :: PlayerEventProcessor
  }

data Player' = 
  Player'
  { playerName' :: PlayerName
  , eventProcessor' :: forall m . PlayerInterface m => GameEvent -> m Player'
  }

-- main entry point
runGame :: [Player] -> IO ()
runGame players = do
  -- create game state monad on top of IO
  State.evalStateT (startController players) emptyGameState

runGame' :: [Player'] -> IO ()
runGame' players = do
  -- create game state monad on top of IO
  State.evalStateT (startController' players) emptyGameState

type HasGameState m = MonadState GameState m

type GameInterface m = (MonadState GameState m, MonadWriter [GameEvent] m)

type ControllerInterface m = (MonadIO m, MonadState GameState m)

startController :: ControllerInterface m => [Player] -> m ()
startController players = do
  -- setup game state
  let playerNames = map playerName players
  State.modify (\state -> state { gameStatePlayers = playerNames,
                                  gameStateStacks  = Map.fromList (zip playerNames $ repeat Set.empty)
                                })
  shuffledCards <- liftIO $ Shuffle.shuffleRounds 10 Cards.deck
  let hands = Map.fromList (zip playerNames (map Set.fromList (Shuffle.distribute (length playerNames) shuffledCards)))
  gameController players [DealHands hands]

startController' :: ControllerInterface m => [Player'] -> m ()
startController' players = do
  -- setup game state
  let playerNames = map playerName' players
  State.modify (\state -> state { gameStatePlayers = playerNames,
                                  gameStateStacks  = Map.fromList (zip playerNames $ repeat Set.empty)
                                })
  shuffledCards <- liftIO $ Shuffle.shuffleRounds 10 Cards.deck
  let hands = Map.fromList (zip playerNames (map Set.fromList (Shuffle.distribute (length playerNames) shuffledCards)))
  gameController' players [DealHands hands]

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
  fmap gameStateTrick State.get

gameOverM :: HasGameState m => m Bool
gameOverM =
  fmap gameOver State.get

runPlayer' :: PlayerInterface m => Player' -> GameEvent -> m Player'
runPlayer' (Player' p f) gameEvent = f gameEvent

runPlayer :: PlayerInterface m => PlayerEventProcessor -> GameEvent -> m PlayerEventProcessor
runPlayer (PlayerEventProcessor f) gameEvent =
  f gameEvent

announceEvent :: ControllerInterface m => GameEvent -> m ()
announceEvent (PlayerTurn playerName) =
  liftIO $ putStrLn ("Your turn, " ++ playerName ++ "!")
announceEvent (CardPlayed playerName card) =
  liftIO $ putStrLn (playerName ++ " plays " ++ pretty card)
announceEvent (TrickTaken playerName trick) =
  liftIO $ putStrLn (playerName ++ " takes the trick:\n" ++ pretty (cardsOfTrick trick))
announceEvent (IllegalMove playerName) =
  liftIO $ putStrLn (playerName ++ " tried to play an illegal card")
announceEvent (GameOver) = do
  liftIO $ putStrLn "Game Over"
  gameState <- State.get
  let stacks = gameStateStacks gameState
      playerPenalties = Map.map penalty stacks
      loserName = fst $ Map.foldrWithKey (\playerName points (loserName, loserPoints) -> 
                                          if points >= loserPoints
                                          then (playerName, points)
                                          else (loserName, loserPoints)
                                       ) ("", 0) playerPenalties
      stackInfo (playerName, points) = do
        putStr playerName
        putStr " has "
        putStr (show points)
        putStrLn " points."
  liftIO $ mapM_ stackInfo (Map.assocs playerPenalties)
  liftIO $ putStrLn (loserName ++ " lost the game.")
announceEvent gameEvent =
  liftIO $ putStrLn (show gameEvent)

gameController :: ControllerInterface m => [Player] -> [GameCommand] -> m ()
gameController players commands = do
  -- traceM ("** INCOMING COMMANDS " ++ show commands) 
  events <- Writer.execWriterT $ mapM_ processGameCommandM' commands
  mapM_ announceEvent events
  -- st <- State.get
  -- traceM ("** GAMESTATE: " ++ show st)
  -- traceM ("** OUTGOING EVENTS " ++ show events)
  (players', commands') <- Writer.runWriterT $ mapM
    (\pp -> do cp' <- Foldable.foldlM runPlayer (eventProcessor pp) events
               return pp { eventProcessor = cp' }
    ) players
  unless (null commands') $
    gameController players' commands'

gameController' :: ControllerInterface m => [Player'] -> [GameCommand] -> m ()
gameController' players commands = do
  -- traceM ("** INCOMING COMMANDS " ++ show commands) 
  events <- Writer.execWriterT $ mapM_ processGameCommandM' commands
  mapM_ announceEvent events
  -- st <- State.get
  -- traceM ("** GAMESTATE: " ++ show st)
  -- traceM ("** OUTGOING EVENTS " ++ show events)
  (players', commands') <- Writer.runWriterT $ 
    mapM (\player -> Foldable.foldlM runPlayer' player events) players
  unless (null commands') $
    gameController' players' commands'

processGameCommandM :: GameInterface m => GameCommand -> m ()
processGameCommandM command =
  do gameState <- State.get
     let (gameState', events) = processGameCommand command gameState
     State.put gameState'
     Writer.tell events

-- directly monadic version
processGameCommandM' :: GameInterface m => GameCommand -> m ()
processGameCommandM' (DealHands playerHands) =
   processAndPublishEventM (HandsDealt playerHands)
processGameCommandM' (PlayCard playerName card) =
   do playIsValid <- playValidM playerName card
      if playIsValid then
        do processAndPublishEventM (CardPlayed playerName card)
           turnIsOver <- turnOverM
           if turnIsOver then
             do trick <- currentTrickM
                let trickTaker = whoTakesTrick trick
                processAndPublishEventM (TrickTaken trickTaker trick)
                gameIsOver <- gameOverM
                if gameIsOver 
                then processAndPublishEventM (GameOver)
                else processAndPublishEventM (PlayerTurn trickTaker)
           else
             do nextPlayer <- nextPlayerM
                processAndPublishEventM (PlayerTurn nextPlayer)
      else
        do nextPlayer <- nextPlayerM
           processAndPublishEventM (IllegalMove nextPlayer)
           processAndPublishEventM (PlayerTurn nextPlayer)

elseM = id

-- fully monadicized version
processGameCommandM'' :: GameInterface m => GameCommand -> m ()
processGameCommandM'' (DealHands playerHands) =
  processAndPublishEventM (HandsDealt playerHands)
processGameCommandM'' (PlayCard playerName card) =
  ifM (playValidM playerName card)
    (do
      processAndPublishEventM (CardPlayed playerName card)
      ifM turnOverM
        (do
          trick <- currentTrickM
          let trickTaker = whoTakesTrick trick
          processAndPublishEventM (TrickTaken trickTaker trick)
          ifM gameOverM
            (processAndPublishEventM (GameOver))
            (processAndPublishEventM (PlayerTurn trickTaker)))
       (do                     -- not turnOver
         nextPlayer <- nextPlayerM
         processAndPublishEventM (PlayerTurn nextPlayer)))
    (do                        -- not playValid
      nextPlayer <- nextPlayerM
      processAndPublishEventM (IllegalMove nextPlayer)
      processAndPublishEventM (PlayerTurn nextPlayer))


processAndPublishEventM :: GameInterface m => GameEvent -> m ()
processAndPublishEventM gameEvent = do
  processGameEventM gameEvent
  Writer.tell [gameEvent]

processGameEventM :: GameInterface m => GameEvent -> m ()
processGameEventM (HandsDealt playerHands) =
  do gameState <- State.get
     State.put (gameState { gameStateHands = playerHands })

processGameEventM (CardPlayed playerName card) =
  State.modify (\state ->
                   state { gameStatePlayers = rotate (rotateTo playerName (gameStatePlayers state)),
                           gameStateHands   = takeCard (gameStateHands state) playerName card,
                           gameStateStacks  = gameStateStacks state,
                           gameStateTrick   = addToTrick playerName card (gameStateTrick state)
                         })

processGameEventM (PlayerTurn playerName) = 
  State.modify (\state -> state { gameStatePlayers = rotateTo playerName (gameStatePlayers state) })

processGameEventM (TrickTaken playerName trick) =
  State.modify (\state -> state { gameStateStacks = (addToStack (gameStateStacks state) playerName (cardsOfTrick trick)),
                                  gameStateTrick = emptyTrick
                                })

processGameEventM (IllegalMove playerName) =
  return ()

processGameEventM (GameOver) =
  return ()

--------------------------------------------------------------------------------
-- players

type HasPlayerState m = MonadState PlayerState m

modifyHand  f = State.modify (\playerState -> playerState { playerHand = f (playerHand playerState)})
modifyTrick f = State.modify (\playerState -> playerState { playerTrick = f (playerTrick playerState)})
modifyStack f = State.modify (\playerState -> playerState { playerStack = f (playerStack playerState)})

playerProcessGameEventM :: (HasPlayerState m, PlayerInterface m) => PlayerName -> GameEvent -> m ()
playerProcessGameEventM playerName gameEvent = do
  case gameEvent of
    HandsDealt hands ->
      State.put (PlayerState { playerHand = hands ! playerName,
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

    IllegalMove playerName ->
      return ()

    GameOver ->
      return ()
  -- st <- State.get
  -- traceM ("** AFTER PLAYERPROCESSGAMEEVENT " ++ playerName ++ " " ++ show gameEvent ++ ": " ++ show st)

makePlayer :: PlayerName -> PlayerStrategy -> Player
makePlayer playerName strategy =
  Player playerName $ strategyPlayer playerName strategy emptyPlayerState

strategyPlayer :: PlayerName -> PlayerStrategy -> PlayerState -> PlayerEventProcessor
strategyPlayer playerName strategy playerState =
  PlayerEventProcessor $ \ event -> do
    nextPlayerState <- flip State.execStateT playerState $ do
      playerProcessGameEventM playerName event
      playerState <- State.get
      case event of
        HandsDealt _ ->
          when (Set.member twoOfClubs (playerHand playerState)) $
            Writer.tell [PlayCard playerName twoOfClubs]

        PlayerTurn turnPlayerName ->
          when (playerName == turnPlayerName) $ do
            card <- chooseCard strategy
            Writer.tell [PlayCard playerName card]

        CardPlayed _ _ ->
          return ()

        TrickTaken _ _ ->
          return ()

        IllegalMove _ ->
          return ()

        GameOver ->
          return ()

    return (strategyPlayer playerName strategy nextPlayerState)

{-
playAlongPlayer :: PlayerName -> PlayerState -> PlayerEventProcessor
playAlongPlayer playerName playerState =
  PlayerEventProcessor $ \ event -> do
    nextPlayerState <- flip State.execStateT playerState $ do
      playerProcessGameEventM playerName event
      playerState <- State.get
      case event of
        HandsDealt _ ->
          when (Set.member twoOfClubs (playerHand playerState)) $
            Writer.tell [PlayCard playerName twoOfClubs]

        PlayerTurn turnPlayerName ->
          when (playerName == turnPlayerName) $ do
            card <- playAlongStrategy'
            Writer.tell [PlayCard playerName card]

        _ -> return ()
    return (playAlongPlayer playerName nextPlayerState)
-}

makePlayer' :: PlayerName -> PlayerStrategy -> Player'
makePlayer' playerName strategy =
  strategyPlayer' playerName strategy emptyPlayerState

strategyPlayer' :: PlayerName -> PlayerStrategy -> PlayerState -> Player'
strategyPlayer' playerName strategy playerState =
  Player' playerName $ \ event -> do
    nextPlayerState <- flip State.execStateT playerState $ do
      playerProcessGameEventM playerName event
      playerState <- State.get
      case event of
        HandsDealt _ ->
          when (Set.member twoOfClubs (playerHand playerState)) $
            Writer.tell [PlayCard playerName twoOfClubs]

        PlayerTurn turnPlayerName ->
          when (playerName == turnPlayerName) $ do
            card <- chooseCard strategy
            Writer.tell [PlayCard playerName card]

        CardPlayed _ _ ->
          return ()

        TrickTaken _ _ ->
          return ()

        IllegalMove _ ->
          return ()

        GameOver ->
          return ()

    return (strategyPlayer' playerName strategy nextPlayerState)

playAlongProcessEventM :: (MonadState PlayerState m, PlayerInterface m) => PlayerName -> GameEvent -> m ()
playAlongProcessEventM playerName event =
  do playerProcessGameEventM playerName event
     playerState <- State.get
     case event of
       HandsDealt _ ->
         if Set.member twoOfClubs (playerHand playerState)
         then Writer.tell [PlayCard playerName twoOfClubs]
         else return ()

       PlayerTurn turnPlayerName ->
         if playerName == turnPlayerName
         then do card <- playAlongCard'
                 Writer.tell [PlayCard playerName card]
         else  return ()

       _ -> return ()

playAlongPlayer' :: PlayerName -> PlayerState -> Player'
playAlongPlayer' playerName playerState =
  let nextPlayerM event =
        do nextPlayerState <- State.execStateT (playAlongProcessEventM playerName event) playerState
           return (playAlongPlayer' playerName nextPlayerState)
  in Player' playerName nextPlayerM

-- stupid robo player
playAlongStrategy :: PlayerStrategy
playAlongStrategy =
  PlayerStrategy $ do
  playerState <- State.get
  let trick = playerTrick playerState
      hand = playerHand playerState
      firstCard = leadingCardOfTrick trick
      firstSuit = suit firstCard
      followingCardsOnHand = Set.filter ((== firstSuit) . suit) hand
  if trickEmpty trick 
    then
      return (Set.findMin hand)
    else
      case Set.lookupMin followingCardsOnHand of
        Nothing ->
          return (Set.findMax hand) -- any card is fine, so try to get rid of high hearts
        Just card ->
          return card           -- otherwise use the minimal following card

playAlongCard' :: (HasPlayerState m, MonadIO m) => m Card
playAlongCard' =
 do
  playerState <- State.get
  let trick = playerTrick playerState
      hand = playerHand playerState
      firstCard = leadingCardOfTrick trick
      firstSuit = suit firstCard
      followingCardsOnHand = Set.filter ((== firstSuit) . suit) hand
  if trickEmpty trick 
    then
      return (Set.findMin hand)
    else
      case Set.lookupMin followingCardsOnHand of
        Nothing ->
          return (Set.findMax hand) -- any card is fine, so try to get rid of high hearts
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
  let myhand = Set.elems hand
      ncards = Set.size hand
  liftIO $ putStrLn ("Your hand:")
  liftIO $ putStrLn (pretty (zip [(1::Integer)..] myhand))
  liftIO $ putStrLn ("Pick a card (1-" ++ show ncards ++ ")")
  selected <- liftIO $ getNumber (1,ncards)
  return (myhand !! (selected - 1))
      
playerMike = makePlayer "Mike" playAlongStrategy
playerPeter = makePlayer "Peter" playInteractive
playerAnnette = makePlayer "Annette" playAlongStrategy
playerNicole = makePlayer "Nicole" playAlongStrategy

start :: IO ()
start = runGame [playerNicole, playerAnnette, playerPeter, playerMike]

playerMike' = makePlayer' "Mike" playAlongStrategy
playerPeter' = makePlayer' "Peter" playInteractive
playerAnnette' = makePlayer' "Annette" playAlongStrategy
playerNicole' = makePlayer' "Nicole" playAlongStrategy

start' :: IO ()
start' = runGame' [playerNicole', playerAnnette', playerPeter', playerMike']

{-
remote: 
remote: Create a pull request for 'peters-alt' on GitHub by visiting:        
remote:      https://github.com/funktionale-programmierung/java-magazin-2019/pull/new/peters-alt        
remote: 
-}

