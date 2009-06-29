{-# LANGUAGE BangPatterns #-}
module Simulation where

import Math
import Orbits
import System.Environment

gt :: Position -> Velocity
gt pos@(!x,!y) = (k_mu / (x^2+y^2)) `pMul` (normVect . negVect $ pos) -- Vector to earth of mag gt

earthCollisionCheck :: Position -> Bool
earthCollisionCheck p@(x,y) =
    (abs x <= radius_e && abs y <= radius_e && x^2 + y^2 < re2)
    where
      re2 = radius_e ^ 2

-- Pos, cur vel, accel, new pos and vel
stepPos :: Position -> Velocity -> Velocity -> (Position, Velocity)
stepPos pos@(!x,!y) v@(!vx,!vy) dv@(!dvx,!dvy) = (pos', v')
    where
      g = gt pos
      g' = gt pos'
      pos' = pos `pAdd` v `pAdd` ((1/2) `pMul` (g `pAdd` dv))
      v' = v `pAdd` (dv `pAdd` (g `pAdd` g' `pDiv` 2))

inferVel :: Position -> Position -> Velocity
inferVel st st1 = negVect (st `pSub` st1 `pAdd` g)
  where
    g = gt st

predict :: Position -> Position -> Position
predict p1 p2 = fst $ stepPos p2 (inferVel p1 p2) (0,0)

integratePos :: Position -> Velocity -> Int -> (Position, Velocity)
integratePos pos@(!x,!y) v@(!vx,!vy) 0 = (pos, v)
integratePos pos@(!x,!y) v@(!vx,!vy) n = integratePos pos' v' (n-1)
    where
      (pos', v') = stepPos pos v (0,0)

hitsEarth :: Position -> Velocity -> Int -> Bool
hitsEarth pos v 0 = earthCollisionCheck pos
hitsEarth pos v n = if earthCollisionCheck pos
                     then True
                     else hitsEarth pos' v' (n-1)
    where
      (pos',v') = stepPos pos v (0,0)

testpos = (-6556995.342903, 15629)
testvel = (13.971285, 7814.921637)
--main = do
--  [steps] <- getArgs
--  print $ integratePos testpos testvel (read steps)
