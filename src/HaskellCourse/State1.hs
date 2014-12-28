{-# Language TypeSynonymInstances #-}
{-# Language InstanceSigs #-}

module State1 where

import Control.Applicative

newtype State s a = State { runState :: s -> (a, s) }

instance Functor (State s) where
  fmap :: (a -> b) -> State s a -> State s b
  fmap f x = State $ \s ->
    let (a, s') = runState x s in (f a, s')

instance Applicative (State s) where
  pure :: a -> State s a
  pure a = State $ \s -> (a, s)
  (<*>) :: State s (a -> b) -> State s a -> State s b
  fs <*> x = State $ \s ->
    let (ab, s')  = runState fs s
        (a,  s'') = runState x s'
    in (ab a, s'')

instance Monad (State s) where
  return :: a -> State s a
  return a = State $ \s -> (a, s)
  x >>= k  = State $ \s ->
    let (a, s') = runState x s in (runState $ k a) s'

newtype StateT s m a = StateT { runStateT :: s -> m (a,s) }

instance (Functor m) => Functor (StateT s m) where
  fmap :: (a -> b) -> StateT s m a -> StateT s m b
  fmap f x = StateT $ \s ->
    let mas = runStateT x s in fmap (\(a,s) -> (f a,s)) mas

instance (Functor m, Monad m) => Applicative (StateT s m) where
  pure :: a -> StateT s m a
  pure a = StateT $ \s -> return (a,s)
  (<*>) :: StateT s m (a -> b) -> StateT s m a -> StateT s m b
  fs <*> x = StateT $ \s ->
    let mabs = runStateT fs s
        mab  = fmap fst mabs
        ms'  = fmap snd mabs
        mas  = do s <- ms'; runStateT x s
        ma   = fmap fst mas
        ms'' = fmap snd mas
        mb   = do ab <- mab; a <- ma; return $ ab a
    in do b <- mb; s <- ms''; return (b,s)

instance Monad m => Monad (StateT s m) where
  return a = StateT $ \s -> return (a, s)
  m >>= k  = StateT $ \s -> do
    (a, s') <- runStateT m s
    runStateT (k a) s'

