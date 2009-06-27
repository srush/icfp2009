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
                                  oe_t :: Double, -- time
                                  oe_i :: Double, -- inclination (3d only)
                                  oe_l :: Double -- RAAN (3d only)
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

argumentOfPeriapsis :: Velocity -> Position -> Double
argumentOfPeriapsis v r = acos (ex / vecMag e)
  where
    e@(ex,_) = eccentricityVector v r

toOrbitalElements :: Velocity -> Position -> Double -> OrbitalElems
toOrbitalElements v r t = OrbitalElems a e w t 0 0
    where
      vm = vecMag v
      rm = vecMag r
      a = semiMajor vm rm
      e = eccentricity v r
      w = argumentOfPeriapsis v r

orbitalPeriod :: Double -> Double
orbitalPeriod a = 2 * pi * sqrt (a^3 / k_mu)

meanMotion :: Double -> Double
meanMotion per = 2*pi / per

meanAnomaly :: OrbitalElems -> Double -> Double
meanAnomaly oe t = m * (t - tt)
    where
      m = meanMotion (orbitalPeriod (oe_a oe))
      tt = oe_t oe

eccentricAnomaly :: OrbitalElems -> Double -> Double
eccentricAnomaly oe t = _loop m
    where
      m = meanAnomaly oe t
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
      l = oe_l oe
      i = oe_i oe
      u = eccentricAnomaly oe t
      p'''@(x''',y''') = (a * (cos u - e), a * sqrt (1 - e^2) * sin u)
      (x'',y'') = rotateVect w p'''
      p'@(x',y') = (x'', y'' * cos i)
      p@(x,y) = rotateVect l p'
      q = atan2 y''' x'''
      vf = sqrt (k_mu / (a * (1 - e^2)))
      (vx',vy') = (-sin q * vf, (e + cos q) * vf)
      v' = (vx', vy' * cos i)
      v = rotateVect w . rotateVect l $ v'

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
