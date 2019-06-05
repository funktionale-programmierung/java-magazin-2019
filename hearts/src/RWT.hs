{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module RWT where

import Control.Monad.Trans.Free
import Control.Monad.Trans.Class

import Control.Monad.State.Class (MonadState, get, put)

data RWF state next =
    GetString (String -> next)
  | PutString String next
  | GetState (state -> next)
  | PutState state next

instance Functor (RWF state) where
  fmap f (GetString k) = GetString(f . k)
  fmap f (PutString string next) = PutString string (f next)
  fmap f (GetState k) = GetState (f . k)
  fmap f (PutState state next) = PutState state (f next)

type RWT' state monad a = FreeT (RWF state) monad a

runRWFIO :: (forall a . monad a -> IO a) -> state -> RWT' state monad a -> IO a
runRWFIO lower state action =
  do x <- lower (runFreeT action)
     case x of
       Pure r -> return r
       Free (GetState k) ->
         runRWFIO lower state (k state)
       Free (PutState state action') ->
         runRWFIO lower state action'
       Free (GetString k) ->
         do string <- getLine
            runRWFIO lower state (k string)
       Free (PutString string action') ->
         do putStrLn string
            runRWFIO lower state action'

newtype RWT state monad a = RWT { unRWT :: RWT' state monad a }
  deriving (Functor, Applicative, Monad, MonadTrans)

putString :: Monad monad => String -> RWT state monad ()
putString string = RWT (liftF (PutString string ()))

getString :: Monad monad => RWT state monad String
getString = RWT (liftF (GetString id))

getState :: Monad monad => RWT state monad state
getState = RWT (liftF (GetState id))

putState :: Monad monad => state -> RWT state monad ()
putState state = RWT (liftF (PutState state ()))

instance Monad monad => MonadState state (RWT state monad) where
  get = getState
  put = putState

runRWTIO :: (forall a . monad a -> IO a) -> state -> RWT state monad a -> IO a
runRWTIO lower state action = runRWFIO lower state (unRWT action)

-- |read number in given range from terminal
getNumber :: (Num a, Ord a, Read a, Show a, Monad monad) => (a, a) -> RWT state monad a
getNumber (lo, hi) = do
  s <- getString
  let input = read s
  if lo <= input && input <= hi
  then return input
  else
    do putString ("Input must be between " ++ (show lo) ++ " and " ++ (show hi) ++ ". Try again")
       getNumber (lo, hi)
