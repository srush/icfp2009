module Util where

import Data.IORef
import Control.Monad.State
import Control.Exception

doUntil :: Monad m => m Bool -> m ()
doUntil m = do
  v <- m
  if v then doUntil m else return ()

encapsulateState :: (b -> State s a) -> s -> IO (b -> IO a)
encapsulateState st init = do
  ios <- newIORef init
  return $ \p -> do
    s <- readIORef ios
    let (v, s') = runState (st p) s
    writeIORef ios s'
    return v

ignoreExns :: IO () -> IO ()
ignoreExns a = handle h a
    where
      h :: SomeException -> IO ()
      h e = return ()

loopM :: Monad m => Int -> (a -> m a) -> a -> m a
loopM 0 _ v = return v
loopM n m v = do
  v' <- m v
  loopM (n-1) m v'

type Counter = (Int, IORef Int)
newCounter :: Int -> IO Counter
newCounter n = do
  r <- newIORef 0
  return (n, r)

bumpCounter :: Counter -> IO Bool
bumpCounter (max, r) = do
  v <- readIORef r
  if v == max then do
              writeIORef r 0
              return True
   else do
     writeIORef r (v+1)
     return False

everyN :: Int -> (a -> IO ()) -> IO (a -> IO ())
everyN n a = do
  c <- newCounter n
  return $ \v -> do
               t <- bumpCounter c
               if t then a v else return ()