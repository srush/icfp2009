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

type Alarm = (Int, IORef Int)
newAlarm :: Int -> IO Alarm
newAlarm n = do
  r <- newIORef 0
  return (n, r)

bumpAlarm :: Alarm -> IO Bool
bumpAlarm (max, r) = do
  v <- readIORef r
  if v == max then do
              writeIORef r 0
              return True
   else do
     writeIORef r (v+1)
     return False

everyN :: Int -> (a -> IO ()) -> IO (a -> IO ())
everyN n a = do
  c <- newAlarm n
  return $ \v -> do
               t <- bumpAlarm c
               if t then a v else return ()

newCounter :: IO (IORef Int)
newCounter = newIORef (-1)

bumpCounter :: IORef Int -> IO Int
bumpCounter i = do
  c <- readIORef i
  writeIORef i (c+1)
  return c

newTrashCan :: IO (IORef [a])
newTrashCan = newIORef []

putItInTheTrash :: IORef [a] -> a -> IO ()
putItInTheTrash i x = do
  modifyIORef i (x:)

emptyTheTrash :: IORef [a] -> IO [a]
emptyTheTrash i = do
  v <- readIORef i
  return $ reverse v
