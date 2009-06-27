module Prob1 where

import Orbits
import Client
import Interpreter
import Data.Map
import qualified Port as P
import Control.Monad.State

data Prob1State = ZeroState
                | PrimaryState Position
                | SecondaryState {s_time :: Int, s_v2 :: Velocity,
                                  s_ttime :: Int}
                | RestState
                  deriving Show



prob1client :: P.Port -> State Prob1State ClientResult
prob1client p = do
  let (f, x, y, scr) = P.readStd p
  cur_state <- get
  case cur_state of
    ZeroState -> do
                  put $ PrimaryState (x,y)
                  retPort P.inert
    PrimaryState oldPos -> do
                   let (v1, v2, tm) = hohmannV (x,y) (clockwise oldPos (x,y))
                                      (P.readD0 4 p)
                   put $ SecondaryState 0 v2 (round tm)
                   retPort $ P.burn v1
    SecondaryState {s_time = time, s_v2 = v2, s_ttime = ttime} -> do
                   let curTime = time + 1
                   if curTime == ttime then
                       do
                         put RestState
                         retPort $ P.burn v2
                     else
                       do
                         put $ cur_state {s_time = curTime}
                         retPort P.inert
    RestState -> if scr == 1.0 then return Finished else retPort P.inert

runProb1 :: String -> Int -> Double -> IO Double
runProb1 host port cfg = runStateClient host port cfg prob1client ZeroState
