{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
module ITree where

-- super structure of computation with events
data ITree :: (* -> *) -> * -> * where
  Ret :: forall event r . r -> ITree event r
  Vis :: forall event r x . event x -> (x -> ITree event r) -> ITree event r
  Tau :: forall event r . ITree event r -> ITree event r

-- interpreter of event structure in a monad
interp :: Monad m => (forall a . event a -> m a) -> (ITree event t -> m t)
interp h (Ret r) = return r
interp h (Vis ev k) = h ev >>= (interp h . k)
interp h (Tau ev) = interp h ev

-- events
data GameEvent :: * -> * where
  Init :: [String] -> GameEvent ()
  Shuffle :: GameEvent ()
  PlayCard :: Int -> GameEvent ()
  WhoGotTheTrick :: GameEvent Int
  GameOver :: GameEvent Bool

-- event-based game specification
game players =
  Vis (Init players) $ \ () ->
  Vis Shuffle $ \ () ->
  loop 0
 where
  loop leader =
    Vis GameOver $ \ gameOver ->
    if gameOver
    then Ret ()
    else
      Vis (PlayCard leader) $ \ () ->
      Vis (PlayCard ((leader + 1) `mod` 4)) $ \ () ->
      Vis (PlayCard ((leader + 2) `mod` 4)) $ \ () ->
      Vis (PlayCard ((leader + 3) `mod` 4)) $ \ () ->
      Vis WhoGotTheTrick $ loop

-- missing: interpretation of game events in a suitable monad
realization :: Monad m => GameEvent t -> m t
realization (Init players) = undefined -- store player names in state
                             -- perhaps better store player strategies
realization Shuffle = undefined        -- shuffle deck of cards and distribute
realization (PlayCard i) = undefined   -- run strategy of player i on current trick
realization WhoGotTheTrick = undefined -- move trick to player who earned it
                             -- and return its number
realization GameOver = undefined -- check for game over
                       -- last two events could be combined

--------------------------------------------------------------------------------
-- optional stuff
--------------------------------------------------------------------------------
  
data Event :: (* -> *) -> * where
  Event :: forall event x . event x -> x -> Event event

type Trace event
  = [Event event]

instance Functor (ITree event) where
  fmap g (Ret r) = Ret (g r)
  fmap g (Vis ev k) = Vis ev (fmap g . k)
  fmap g (Tau t) = Tau (fmap g t)

instance Applicative (ITree event) where
  pure = Ret
  Ret g <*> fa = fmap g fa
  Vis ev k <*> fa = Vis ev (\ x -> k x <*> fa)
  Tau t <*> fa = Tau (t <*> fa)

instance Monad (ITree event) where
  Ret x >>= g = g x
  Vis ev k >>= g = Vis ev (\ x -> k x >>= g)
  Tau t >>= g = Tau (t >>= g)

