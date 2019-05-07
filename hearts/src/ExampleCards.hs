module ExampleCards where

import Cards

-- |example cards
aceOfSpades = Card Ace Spades
tenOfHearts = Card (Numeric 10) Hearts
queenOfHearts = Card Queen Hearts
jackOfClub = Card Jack Clubs

exampleHand = [jackOfClub, queenOfHearts, tenOfHearts, aceOfSpades]
tenOfClubs = Card (Numeric 10) Clubs
