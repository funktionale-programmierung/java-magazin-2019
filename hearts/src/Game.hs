{-# LANGUAGE TupleSections #-}
module Game where

import qualified Data.Set as Set
import Data.Set (Set)

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map, (!))
import Cards

import Debug.Trace (trace)

-- start card
twoOfClubs = Card Clubs (Numeric 2)

-- Games
type PlayerName = String

-- * Tricks

-- last card is at the front
type Trick = [(PlayerName, Card)]

emptyTrick :: Trick
emptyTrick = []

trickEmpty :: Trick -> Bool
trickEmpty trick = null trick

trickSize :: Trick -> Int
trickSize trick = length trick

cardsOfTrick :: Trick -> [Card]
cardsOfTrick trick = map snd trick

addToTrick :: PlayerName -> Card -> Trick -> Trick
addToTrick playerName card trick = (playerName, card) : trick

leadingCardOfTrick :: Trick -> Card
leadingCardOfTrick trick = snd (last trick)

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
             || all ((/= firstSuit) . suit) hand -- ok if no such suit in hand

-- * Games

type PlayerStacks = Map PlayerName (Set Card)
type PlayerHands  = Map PlayerName Hand

data GameState =
  GameState
  { gameStatePlayers :: [PlayerName],
    gameStateHands   :: PlayerHands,
    gameStateStacks  :: PlayerStacks,
    gameStateTrick   :: Trick
  }
  deriving Show

emptyGameState = GameState [] Map.empty Map.empty []

gameAtBeginning :: GameState -> Bool
gameAtBeginning gameState =
  (trickEmpty (gameStateTrick gameState)) && (all null (Map.elems (gameStateStacks gameState)))

computeNextPlayer :: PlayerName -> [PlayerName] -> PlayerName
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
  head (gameStatePlayers state)

playValid :: GameState -> PlayerName -> Card -> Bool
playValid gameState playerName card =
  -- FIXME: validate that the card is valid for the trick
  let hand = gameStateHands gameState ! playerName
      trick = gameStateTrick gameState
  in
  legalCard card hand trick &&
  if gameAtBeginning gameState
  then card == twoOfClubs
  else nextPlayer gameState == playerName

gameOver :: GameState -> Bool
gameOver state = all isHandEmpty $ Map.elems $ gameStateHands state

turnOver :: GameState -> Bool
turnOver state = Map.size (gameStateHands state) == trickSize (gameStateTrick state)

data GameEvent =
    HandsDealt (Map PlayerName Hand)
  | PlayerTurn PlayerName
  | CardPlayed PlayerName Card
  | TrickTaken PlayerName Trick
  | IllegalMove PlayerName
  | GameOver
  deriving Show

data GameCommand =
    DealHands (Map PlayerName Hand)
  | PlayCard PlayerName Card
  deriving Show

takeCard :: PlayerHands -> PlayerName -> Card -> PlayerHands
takeCard playerHand player card =
  Map.alter (fmap (removeCard card)) player playerHand

addToStack :: PlayerStacks -> PlayerName -> [Card] -> PlayerStacks
addToStack playerStack player cards =
  Map.alter (fmap (Set.union (Set.fromList cards))) player playerStack

processGameEvent :: GameState -> GameEvent -> GameState
processGameEvent state event | trace ("processGameEvent " ++ show state ++ " " ++ show event) False = undefined
processGameEvent state (HandsDealt hands) =
  GameState { gameStatePlayers = Map.keys hands,
              gameStateHands = hands,
              gameStateStacks = Map.fromList (map (, Set.empty) (Map.keys hands)),
              gameStateTrick = emptyTrick }
processGameEvent state (PlayerTurn player) =
  state { gameStatePlayers = rotateTo player (gameStatePlayers state) }
processGameEvent state (CardPlayed player card) =
  GameState { gameStatePlayers = rotate (rotateTo player (gameStatePlayers state)),
              gameStateHands = takeCard (gameStateHands state) player card,
              gameStateStacks = gameStateStacks state,
              gameStateTrick = addToTrick player card (gameStateTrick state) }
processGameEvent state (TrickTaken player trick) =
  state { gameStateStacks = trace (show "addToStack " ++ show player ++ " " ++ show (cardsOfTrick trick) ++ " " ++ show (addToStack (gameStateStacks state) player (cardsOfTrick trick)))
                            (addToStack (gameStateStacks state) player (cardsOfTrick trick)),
          gameStateTrick = emptyTrick }

data PlayerState =
  PlayerState { playerHand  :: Hand,
                playerTrick :: Trick,
                playerStack :: [Card] }
  deriving Show

emptyPlayerState = PlayerState emptyHand [] []

playerProcessGameEvent :: PlayerName -> PlayerState -> GameEvent -> PlayerState
playerProcessGameEvent playerName state (HandsDealt hands) =
  PlayerState { playerHand = hands ! playerName,
                playerTrick = emptyTrick,
                playerStack = [] }
playerProcessGameEvent playerName state (PlayerTurn playerName') = state
playerProcessGameEvent playerName state (CardPlayed player card)
  | player == playerName =
    state { playerHand = removeCard card (playerHand state),
            playerTrick = addToTrick player card (playerTrick state) }
  | otherwise =
    state { playerTrick = addToTrick player card (playerTrick state) }
playerProcessGameEvent playerName state (TrickTaken player trick)
  | player == playerName =
    state { playerTrick = emptyTrick,
            playerStack = (cardsOfTrick trick) ++ (playerStack state) }
  | otherwise =
    state { playerTrick = emptyTrick }

processGameCommand :: GameState -> GameCommand -> (GameState, [GameEvent])
processGameCommand state command | trace ("processGameCommand " ++ show (gameAtBeginning state) ++ " " ++ show command ++ " " ++ show state) False = undefined
processGameCommand state (DealHands hands) =
  let event = HandsDealt hands
  in (processGameEvent state event, [event])
processGameCommand state (PlayCard player card) =
  if playValid state player card
  then
    let event1 = CardPlayed player card
        state1 = processGameEvent state event1
    in  if turnOver state1 then
          let trick = gameStateTrick state1
              trickTaker = whoTakesTrick trick
              event2 = TrickTaken trickTaker trick
              state2 = processGameEvent state event2
              event3 = if gameOver state
                       then GameOver
                       else PlayerTurn trickTaker
              state3 = processGameEvent state event3
          in (state3, [event1, event2, event3])
        else
          let event2 = PlayerTurn (nextPlayer state1)
              state2 = processGameEvent state event2
          in (state2, [event1, event2])
  else
    (state, [IllegalMove player, PlayerTurn player])

--------------------------------------------------------------------------------
-- general utility

-- |rotate assumes length of input > 0
rotate :: [a] -> [a]
rotate (x : xs) = xs ++ [x]
rotate [] = undefined

-- |rotateTo assumes target exists in input of length > 0
rotateTo :: Eq a => a -> [a] -> [a]
rotateTo y xs@(x : xs') | x == y = xs
                        | otherwise = rotateTo y (xs' ++ [x])
rotateTo y [] = undefined

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
