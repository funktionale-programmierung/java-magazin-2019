module ExampleCards where

import Cards

-- |example cards
aceOfSpades = Card Spades Ace
tenOfHearts = Card Hearts (Numeric 10)
queenOfHearts = Card Hearts Queen
jackOfClub = Card Clubs Jack

exampleHand = [jackOfClub, queenOfHearts, tenOfHearts, aceOfSpades]
tenOfClubs = Card Clubs (Numeric 10)
