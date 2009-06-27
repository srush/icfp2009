module Orbits where

import Math

type Position = (Double, Double)
type Velocity = (Double, Double)


dist :: Position -> Position -> Double
dist (x1, y1) (x2, y2) = sqrt ((x1 - x2) ^ 2 + (y1 - y2) ^2)

-- kg
mass_e :: Double
mass_e = 6.0e24

radius_e :: Double
radius_e = 6.357e6

-- Distance factor in meters
k_G :: Double
k_G = 6.67428e-11

k_mu :: Double
k_mu = k_G * mass_e

-- Current orbital radius -> Target orbital radius -> (dV, dV', Th)
hohmann :: Double -> Double -> (Double, Double, Double)
hohmann r1 r2 = (dv, dv', th)
  where
    r12 = r1 + r2
    dv = sqrt (k_mu / r1) * (sqrt (2*r2 / r12) - 1)
    dv' = sqrt (k_mu / r2) * (1 - sqrt (2*r1 / r12))
    th = pi * sqrt (r12 ^ 3 / (8 * k_mu))

tr1 = 42164000
tr2 = 6678000
(tdv, tdv', tth) = hohmann tr1 tr2

visViva :: Double -> Double -> Double
visViva r a = sqrt $ k_mu * (2 / r - 1 / a)

visVivaCirc :: Double -> Double
visVivaCirc r = visViva r r

-- Your pos -> going clockwise? -> Target radius -> (vel, vel', time)
hohmannV :: Position -> Bool -> Double -> (Velocity, Velocity, Double)
hohmannV (sx, sy) cw r2 = (vel1, vel2, th)
  where
    r1 = dist (sx, sy) (0, 0)
    (dV, dV', th) = hohmann r1 r2
    ang = atan (sy / sx) + (if cw then -pi / 2 else pi / 2)
    vel1 = (dV * cos ang, dV * sin ang)
    vel2 = (dV' * cos ang, dV' * sin ang)

hohmannInt :: Position -> Bool -> Double -> (Velocity, Velocity, Double, Double)
hohmannInt s cw r2 = (v1, v2, r2', int_th)
    where
      (_, _, th) = hohmannV s cw r2
      round_th = fromIntegral . floor $ th
      r1 = vecMag s
      r2' = fromIntegral . round $ (cubeRoot (8 * k_mu * (round_th / pi)^2) - r1)
      (v1, v2, int_th) = hohmannV s cw r2'

(tv, tv', tr2', th) = hohmannInt (tr1, 0) True tr2

posAng :: Position -> Double
posAng (x,y) = atan (y/x)

clockwise :: Position -> Position -> Bool
clockwise p1 p2 = alpha < 0
  where
    alpha = normAng $ posAng p2 - posAng p1

nudgeVel :: Double -> Double -> Double
nudgeVel r1 r2 = (r2 - r1) / 5

-- Inclination is 0 in 2d, and there is no ascending node
data OrbitalElems = OrbitalElems {oe_a :: Double, -- semimajor axis
                                  oe_e :: Double, -- eccentricity
                                  oe_w :: Double, -- argument of periapsis
                                  oe_t :: Double} -- time

semiMajor :: Double -> Double -> Double
semiMajor v r = -k_mu / (2 * e)
  where
    e = v^2 / 2 - k_mu / r

eccentricityVector :: Velocity -> Position -> (Double, Double)
eccentricityVector v r = t1 `pSub`t2 `pSub`(r `pDiv` rm)
  where
    t1 = (vm^2) `pMul` r `pDiv` k_mu
    t2 = ((r `dot` v) `pMul` v) `pDiv` k_mu
    vm = vecMag v
    rm = vecMag r

eccentricity :: Velocity -> Position -> Double
eccentricity v r = vecMag $ eccentricityVector v r

argumentOfPeriapsis :: Velocity -> Position -> Double
argumentOfPeriapsis v r = acos (ex / vecMag e)
  where
    e@(ex,_) = eccentricityVector v r

buildOrbitalElements :: Velocity -> Position -> Double -> OrbitalElems
buildOrbitalElements v r t = OrbitalElems a e w t
    where
      vm = vecMag v
      rm = vecMag r
      a = semiMajor vm rm
      e = eccentricity v r
      w = argumentOfPeriapsis v r
