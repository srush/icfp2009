module Prob1 where

import Orbits
import Client
import Interpreter
import Data.Map
import qualified Port as P
import Control.Monad.State
import Util
import Debug.Trace
import Math

stupidClient :: P.Port -> State Int ClientResult
stupidClient p = do
  t <- get
  put (t+1)
  if t == 10000 then return Finished else retPort P.inert

data HohmannState = ZeroState
                | PrimaryState Position
                | SecondaryState {s_time :: Int, s_v2 :: Velocity,
                                  s_ttime :: Int , s_cw :: Bool }
                | RestState Bool
                  deriving Show

-- Do a hohman - FAILS because you over or undershoot because dt = 1
dumbHohmann :: P.Port -> State HohmannState ClientResult
dumbHohmann p = do
  let (f, x, y, scr) = P.readStd p
  cur_state <- get
  case cur_state of
    ZeroState -> do
                  put $ PrimaryState (-x,-y)
                  let _ = trace "In Zero State, storing init pos" $ traceShow (-x,-y) ()
                  retPort P.inert
    PrimaryState oldPos -> do
                   let (v1, v2, tm) = hohmannV (-x,-y) (clockwise oldPos (-x,-y))
                                      (P.readD0 4 p)
                   put $ SecondaryState 0 v2 (round tm) False
                   retPort $ P.burn v1
    SecondaryState {s_time = time, s_v2 = v2, s_ttime = ttime} -> do
                   let curTime = time + 1
                   if curTime == ttime then
                       do
                         put $ RestState False
                         retPort $ P.burn v2
                     else
                       do
                         put $ cur_state {s_time = curTime}
                         retPort P.inert
    RestState _ -> if scr == 1.0 then return Finished else retPort P.inert

-- Try to keep doing hohmanns that take int steps to do
-- FAILS because it takes forever to do a 1000km hohmann
iterativeHohmann :: P.Port -> State HohmannState ClientResult
iterativeHohmann p = do
  let (f, x, y, scr) = P.readStd p
  let rtarg = P.readD0 4 p
  let rcurr = vecMag (x, y)
  let rdiff = abs (rtarg - rcurr)
  let shiftPrim = put $ PrimaryState (-x, -y)
  cur_state <- get
  case traceShow (rcurr, rtarg, rdiff) cur_state of
    ZeroState -> do
      shiftPrim
      retPort P.inert
    PrimaryState oldPos -> do
                   let (v1, v2, _, tm) = hohmannInt (-x,-y) (clockwise oldPos (-x,-y))
                                         rtarg
                   put $ SecondaryState 0 v2 (round (max 1 tm)) False
                   trace "Initiating burn" $
                           traceShow (v1, v2, tm, rdiff) $
                                     retPort $ P.burn v1
    SecondaryState {s_time = time, s_v2 = v2, s_ttime = ttime} -> do
                   let curTime = time + 1
                   if curTime == ttime then
                       do
                         if rdiff < 1 then
                             trace "Reached target, resting" $ put (RestState False)
                          else
                              trace "Transfer complete, starting another" shiftPrim
                         retPort $ P.burn v2
                     else
                       do
                         put $ cur_state {s_time = curTime}
                         retPort P.inert
    RestState _ ->
        if scr == 1.0 then return Finished
        else do
          if rcurr - rtarg > 1 then trace "Drifted away" shiftPrim
           else return ()
          retPort P.inert

--Do a hohmann and then speed up and slow down to reach target
nudgeHohmann :: P.Port -> State HohmannState ClientResult
nudgeHohmann p = do
  let (f, x, y, scr) = P.readStd p
  let rtarg = P.readD0 4 p
  let rcurr = vecMag (x, y)
  let rdiff = abs (rtarg - rcurr)
  let shiftPrim = put $ PrimaryState (-x, -y)
  cur_state <- get
  case traceShow ((-x,-y), rcurr, rtarg, rdiff) cur_state of
    ZeroState -> do
      shiftPrim
      retPort P.inert
    PrimaryState oldPos -> do
                   let cw = clockwise oldPos (-x,-y)
                   let (v1, v2, _, tm) = hohmannInt (-x,-y) cw rtarg
                   put $ SecondaryState 0 v2 (round (max 1 tm)) cw
                   trace "Initiating burn" $
                           traceShow (v1, v2, tm, rdiff) $
                                     retPort $ P.burn v1
    SecondaryState {s_time = time, s_v2 = v2, s_ttime = ttime, s_cw = cw} -> do
                   let curTime = time + 1
                   if curTime == ttime then
                       do
                         trace "Reached target, nudging" $ put $ RestState cw
                         retPort $ P.burn v2
                     else
                       do
                         put $ cur_state {s_time = curTime}
                         retPort P.inert
    RestState cw ->
        if scr == 1.0 then return Finished
        else do
          if rdiff > 1 then do
                         let nv = nudgeVel rcurr rtarg `pMul`
                                  (normVect . perpVect cw $ (-x,-y))
                         trace "Nudging" $ traceShow nv $
                               retPort $
                                       P.burn nv
           else retPort P.inert



--prob1client = iterativeHohmann
--prob1client = stupidClient
prob1client = nudgeHohmann

main :: IO ()
main = do
  ioc <- encapsulateState prob1client ZeroState
  clientMain ioc
