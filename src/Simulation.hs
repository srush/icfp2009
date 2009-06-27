module Simulation where

import Math
import Orbits

gt :: Position -> Velocity
gt pos = (k_mu / r^2) `pMul` (normVect . negVect $ pos) -- Vector to earth of mag gt
  where
    r = vecMag pos

stepPos :: Position -> Velocity -> Velocity -> Position
stepPos pos v dv = pos `pAdd` v `pAdd` ((1/2) `pMul` (g `pAdd` dv))
    where
      g = gt pos

inferVel :: Position -> Position -> Velocity
inferVel st st1 = negVect (st `pSub` st1 `pAdd` g)
  where
    g = gt st

predict :: Position -> Position -> Position
predict p1 p2 = stepPos p2 (inferVel p1 p2) (0,0)