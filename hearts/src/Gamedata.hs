module Gamedata where

import qualified Data.Map.Strict as M
import qualified Data.Set as S


import Cards

-- start card
twoOfClubs = Card Clubs (Numeric 2)


type PlayerName = String

-- tricks

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

-- Games

type PlayerStacks = M.Map PlayerName [Card]
type PlayerHands  = M.Map PlayerName Hand

data GameState =
  GameState 
  { statePlayers :: [PlayerName],
    stateHands   :: PlayerHands,
    stateStacks  :: PlayerStacks,
    stateTrick   :: Trick
  }
  deriving Show

emptyGameState = GameState [] M.empty M.empty []

gameAtBeginning :: GameState -> Bool
gameAtBeginning gameState =
  (null (stateTrick gameState)) && (all null (M.elems (stateStacks gameState)))

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
  head (statePlayers state)

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
