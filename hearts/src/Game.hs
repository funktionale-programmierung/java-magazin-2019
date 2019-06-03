{-# LANGUAGE TupleSections #-}
module Game where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Cards
import Shuffle

import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Debug.Trace (trace)

-- Games
type PlayerName = String

-- last card is at the front
type Trick = [(PlayerName, Card)]

cardsOfTrick :: Trick -> [Card]
cardsOfTrick = map snd

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

emptyGameState = GameState [] M.empty M.empty []

gameAtBeginning :: GameState -> Bool
gameAtBeginning gameState =
  (null (stateTrick gameState)) && (all null (M.elems (stateStacks gameState)))

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

twoOfClubs = Card Clubs (Numeric 2)

playValid :: GameState -> PlayerName -> Card -> Bool
playValid gameState playerName card =
  -- FIXME: validate that the player has the card, and that its valid for the trick
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

data GameCommand =
    DealHands (M.Map PlayerName Hand)
  | PlayCard PlayerName Card
  deriving Show

takeCard :: PlayerHands -> PlayerName -> Card -> PlayerHands
takeCard playerHand player card =
  M.alter (fmap (removeCard card)) player playerHand

addToStack :: PlayerStacks -> PlayerName -> [Card] -> PlayerStacks
addToStack playerStack player cards =
  M.alter (fmap (cards++)) player playerStack

processGameEvent :: GameState -> GameEvent -> GameState
processGameEvent state event | trace ("processGameEvent " ++ show state ++ " " ++ show event) False = undefined
processGameEvent state (HandsDealt hands) =
  GameState { statePlayers = M.keys hands,
              stateHands = hands,
              stateStacks = M.fromList (map (, []) (M.keys hands)),
              stateTrick = [] }
processGameEvent state (PlayerTurn player) =
  state { statePlayers = rotateTo player (statePlayers state) }
processGameEvent state (CardPlayed player card) =
  GameState { statePlayers = rotate (rotateTo player (statePlayers state)),
              stateHands = takeCard (stateHands state) player card,
              stateStacks = stateStacks state,
              stateTrick = (player, card) : (stateTrick state) }
processGameEvent state (TrickTaken player trick) =
  state { stateStacks = trace (show "addToStack " ++ show player ++ " " ++ show (cardsOfTrick trick) ++ " " ++ show (addToStack (stateStacks state) player (cardsOfTrick trick)))
                            (addToStack (stateStacks state) player (cardsOfTrick trick)),
          stateTrick = [] }

data PlayerState =
  PlayerState { playerHand :: Hand,
                playerTrick :: Trick,
                playerStack :: [Card] }
  deriving Show

playerProcessGameEvent :: PlayerName -> PlayerState -> GameEvent -> PlayerState
playerProcessGameEvent playerName state (HandsDealt hands) =
  PlayerState { playerHand = hands M.! playerName,
                playerTrick = [],
                playerStack = [] }
playerProcessGameEvent playerName state (PlayerTurn playerName') = state
playerProcessGameEvent playerName state (CardPlayed player card)
  | player == playerName =
    state { playerHand = removeCard card (playerHand state),
            playerTrick = (player, card) : (playerTrick state) }
  | otherwise = 
    state { playerTrick = (player, card) : (playerTrick state) }
playerProcessGameEvent playerName state (TrickTaken player trick)
  | player == playerName =
    state { playerTrick = [],
            playerStack = (cardsOfTrick trick) ++ (playerStack state) }
  | otherwise =
    state { playerTrick = [] } 

processGameCommand :: GameState -> GameCommand -> (GameState, [GameEvent])
processGameCommand state command | trace ("processGameCommand " ++ show (gameAtBeginning state) ++ " " ++ show command ++ " " ++ show state) False = undefined
processGameCommand state (DealHands hands) =
  let event = HandsDealt hands
  in (processGameEvent state event, [event])
processGameCommand state (PlayCard player card) =
  if trace (show "processGameCommand valid " ++ show player ++ " " ++ show card ++ " " ++ show (playValid state player card)) (playValid state player card)
  then   
    let event1 = CardPlayed player card
        state1 = processGameEvent state event1
    in  if turnOver state1 then
          let trick = stateTrick state1
              trickTaker = whoTakesTrick trick
              event2 = TrickTaken trickTaker trick
              state2 = processGameEvent state event2
              event3 = PlayerTurn trickTaker
              state3 = processGameEvent state event3
          in (state3, [event1, event2, event3])
        else
          let event2 = PlayerTurn (nextPlayer state1)
              state2 = processGameEvent state event2
          in (state2, [event1, event2])
  else (state, [])


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
