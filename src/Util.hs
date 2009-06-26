module Util where

doUntil :: Monad m => m Bool -> m ()
doUntil m = do
  v <- m
  if v then doUntil m else return ()
