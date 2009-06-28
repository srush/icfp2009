module Orbits where

import Math
import Debug.Trace

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
--k_mu = 1.32712440018e20 -- sun/km

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
                                  oe_m :: Double  -- mean anomaly
                                 }
                  deriving Show

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

eccentricAnomaly :: Velocity -> Position -> Double
eccentricAnomaly v p = acos $ -(r / a - 1) / e
    where
      r = vecMag p
      vm = vecMag v
      e = eccentricity v p
      a = semiMajor vm r

meanAnomaly :: Velocity -> Position -> Double
meanAnomaly v p = u - e * sin u
    where
      u = eccentricAnomaly v p
      e = eccentricity v p

argumentOfPeriapsis :: Velocity -> Position -> Double
argumentOfPeriapsis v r = acos (ex / vecMag e)
  where
    e@(ex,_) = eccentricityVector v r

toOrbitalElements :: Velocity -> Position -> Double -> OrbitalElems
toOrbitalElements v r t = OrbitalElems a e w m
    where
      vm = vecMag v
      rm = vecMag r
      a = semiMajor vm rm
      e = eccentricity v r
      w = argumentOfPeriapsis v r
      u = eccentricAnomaly v r
      m = u - e * sin u

orbitalPeriod :: Double -> Double
orbitalPeriod a = 2 * pi * sqrt (a^3 / k_mu)

meanMotion :: Double -> Double
meanMotion per = 2*pi / per

oeMeanAnomaly :: OrbitalElems -> Double -> Double
oeMeanAnomaly oe t = normAng2Pi $ m * t + (oe_m oe)
    where
      m = meanMotion (orbitalPeriod (oe_a oe))

oeEccentricAnomaly :: OrbitalElems -> Double -> Double
oeEccentricAnomaly oe t = _loop m
    where
      m = oeMeanAnomaly oe t
      _loop u | u' - u < 1e-12 = u'
              | otherwise = _loop u'
        where
          e = oe_e oe
          u' = u + d3
          f0 = u - e * sin u - m
          f1 = 1 - e * cos u
          f2 = e * sin u
          f3 = e * cos u
          d1 = -f0 / f1
          d2 = -f0 * (f1 + d1 * f2 / 2)
          d3 = -f0/(f1 + d1 * f2 / 2 + d2^2 * f3 / 6)

toOrbitalState :: OrbitalElems -> Double -> (Velocity, Position)
toOrbitalState oe t = (v, p)
    where
      a = oe_a oe
      e = oe_e oe
      w = oe_w oe
      u = oeEccentricAnomaly oe t
      p'''@(x''',y''') = (a * (cos u - e), a * sqrt (1 - e^2) * sin u)
      p@(x,y) = rotateVect w p'''
      q = atan2 y''' x'''
      vf = sqrt (k_mu / (a * (1 - e^2)))
      v' = (-sin q * vf, (e + cos q) * vf)
      v = rotateVect w v'

--au2m a = 1.49597870691e11 * a
--jd2s j = (24*60*60) * j

--vesta = OrbitalElems a e w t i l
--    where
--      a = au2m 2.3626478
--      e = 0.08887781
--      i = 0.1245266
--      l = 1.81422
--      w = 2.612391
--      t = jd2s 2452941.1
--vt2 = jd2s 2453040.3
--vestaSt@(vv,vp) = toOrbitalState vesta vt2
--vesta' = toOrbitalElements vv vp vt2

-- List of eccentricities paired with semimajor axis and whether departure is from
-- the apside
eccentricityCases :: Position -> Position -> [(Double, Double, Bool)]
eccentricityCases src trg = filter (\(e,_,_) -> e >= 0 && e < 1) [p1, p2, p3, p4]
    where
      r1 = vecMag src
      r2 = vecMag trg
      d = dist src trg
      ad e = r1 / (1 - e)
      aa e = r1 / (1 - e)
      e1 = 2 * r1 * (r1 - r2) / ( r2^2 - r1^2 - d^2 )
      e2 = 2 * r2 * (r1 - r2) / ( r1^2 - r2^2 - d^2 )
      e3 = 2 * r2 * (r2 - r1) / ( r1^2 - r2^2 - d^2 )
      e4 = 2 * r1 * (r2 - r1) / ( r2^2 - r1^2 - d^2 )
      p1 = (e1, ad e1, True)
      p2 = (e2, aa e2, False)
      p3 = (e3, ad e3, False)
      p4 = (e4, aa e4, True)

-- Relative position (from targ), relative vel (rel to targ), angular vel of targ
dockVels :: Position -> Velocity -> Double -> Double -> Velocity
dockVels (x0,y0) (vx0, vy0) omega tau = (omega * vx / delta - vx0, omega * vy / delta - vy0)
    where
      omgt = omega * tau
      vx = x0 * sin omgt + y0 * (6 * omgt * sin omgt -
                                       14 * (1 - cos omgt))
      vy = 2 * x0 * (1 - cos omgt) + y0 * (4 * sin omgt - 3 * omgt * cos omgt)
      delta = 3 * omgt * sin omgt - 8 * (1 - cos omgt)


angularVel :: Position -> Velocity -> Double
angularVel p v = vm * sin a
    where
      vm = vecMag v
      a = angBetweenVects p v
