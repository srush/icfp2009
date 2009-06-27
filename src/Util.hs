module Util where

import Data.IORef
import Control.Monad.State

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
