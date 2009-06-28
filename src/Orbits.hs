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
                                  oe_m :: Double, -- mean anomaly
                                  oe_cw :: Bool  -- Retrograde or not
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
meanAnomaly v p@(x,y) = normAng2Pi $
                        if e < 1e-13 then atan2 y x else u - e * sin u
    where
      u = eccentricAnomaly v p
      e = eccentricity v p

argumentOfPeriapsis :: Velocity -> Position -> Double
argumentOfPeriapsis v r = a
  where
    cw = clockwise r (r `pAdd` v)
    e@(ex,_) = eccentricityVector v r
    a =  acos (ex / vecMag e)

toOrbitalElementsBad :: Velocity -> Position -> OrbitalElems
toOrbitalElementsBad v r = OrbitalElems a e w m cw
    where
      cw = clockwise r (r `pAdd` v)
      vm = vecMag v
      rm = vecMag r
      a = semiMajor vm rm
      e = eccentricity v r
      w = argumentOfPeriapsis v r
      u = eccentricAnomaly v r
      m = meanAnomaly v r

orbitalPeriod :: Double -> Double
orbitalPeriod a = 2 * pi * sqrt (a^3 / k_mu)

meanMotion :: Double -> Double
meanMotion per = 2*pi / per

oeMeanAnomaly :: OrbitalElems -> Double -> Double
oeMeanAnomaly oe t = normAng2Pi $ m * t + (oe_m oe)
    where
      m = meanMotion (orbitalPeriod (oe_a oe))

oeEccentricAnomaly :: OrbitalElems -> Double -> Double
oeEccentricAnomaly oe t = _loop1 0 m
    where
      k0 = 0.01745329251994329576923691
      e = oe_e oe
      m = oeMeanAnomaly oe t
      _loop1 500 _ = _loop2 0
      _loop1 n u | u' - u < 1e-12 = u'
                 | otherwise = _loop1 (n+1) u'
        where
          u' = u + d3
          f0 = u - e * sin u - m
          f1 = 1 - e * cos u
          f2 = e * sin u
          f3 = e * cos u
          d1 = -f0 / f1
          d2 = -f0 * (f1 + d1 * f2 / 2)
          d3 = -f0/(f1 + d1 * f2 / 2 + d2^2 * f3 / 6)
      _loop2 35 = error "Couldn't find eccentric anomaly"
      _loop2 n | dm0 * dm2 <= 0 = _loop3 u0 u2
               | otherwise = _loop2 (n+1)
          where
            u0 = 10 * n * k0
            u2 = 10 * (n+1) * k0
            m0 = u0 - e * sin u0
            m2 = u2 - e * sin u2
            dm0 = m - m0
            dm2 = m - m2
      _loop3 u0 u2 | u2 - u0 < 1e-13 = u0 + (u2-u0)*(m-m0)/(m2-m0)
                   | otherwise = _loop3 u0' u2'
          where
            u1 = (u0+u2)/2
            m0 = u0 - e * sin u0
            m1 = u1 - e * sin u1
            m2 = u2 - e * sin u2
            dm0 = m - m0
            dm1 = m - m1
            (u0', u2') = if dm0 * dm1 < 0 then (u0, u1) else (u1, u2)

toOrbitalState :: OrbitalElems -> Double -> (Velocity, Position)
toOrbitalState oe t = traceShow (xw, a, eca, ceca, ec) ((xd,yd), (x,y))
    where
      m0 = oe_m oe
      findEca eca | abs (e1 - eca) < 1e-12 = eca
                  | otherwise = findEca e1
          where
            e1 = eca - (eca - ec * sin eca - m0) / (1 - ec * cos eca)
      a = oe_a oe
      ec = oe_e oe
      w0 = oe_w oe
      o0 = 0
      i = if oe_cw oe then pi else 0
      eca = oeEccentricAnomaly oe t
      --eca = findEca (m0 + ec/2)
      ceca=cos eca
      seca=sin eca
      e1=a*sqrt (1-ec*ec)
      xw=a*(ceca-ec)
      yw=e1*seca
      edot=sqrt(k_mu/a)/(a*(1-ec*ceca))
      xdw = -a*edot*seca
      ydw=e1*edot*ceca
      cw = cos w0
      sw=sin w0
      co=cos o0
      so=sin o0
      ci=cos i
      si=sin i
      swci=sw*ci
      cwci=cw*ci
      px=cw*co-so*swci
      py=cw*so+co*swci
      pz=sw*si
      qx= -sw*co-so*cwci
      qy= -sw*so+co*cwci
      qz=cw*si
      x=xw*px+yw*qx
      y=xw*py+yw*qy
      z=xw*pz+yw*qz
      xd=xdw*px+ydw*qx
      yd=xdw*py+ydw*qy
      zd=xdw*pz+ydw*qz

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
dockVel :: Position -> Velocity -> Double -> Double -> Velocity
dockVel (x0,y0) (vx0, vy0) omega tau = (omega * vx / delta - vx0, omega * vy / delta - vy0)
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

stepOrbit :: Position -> Velocity -> Double -> (Velocity, Position)
stepOrbit p v t = toOrbitalState oe t
  where
    oe = toOrbitalElements p v

tp1 :: Position
tp1 = (4e8,0)
tv1 :: Velocity
tv1 = (0,0 - visVivaCirc 4e8 -100)

toe = toOrbitalElements tp1 tv1

getPts :: OrbitalElems -> Double -> Double -> [Position]
getPts oe s m = map (snd . toOrbitalState oe) [0,s..m]

toePts = getPts toe 10000 5000000

toOrbitalElements :: Position -> Velocity -> OrbitalElems
toOrbitalElements (x,y) (xd, yd) = do
  let z=0
  let zd=0
  let rmu1=1.0/k_mu
  let v20 = xd*xd+yd*yd+zd*zd
  let r0=x*x+y*y+z*z
  let r=sqrt r0
  let rv=x*xd+y*yd+z*zd
  let hi=y*zd-z*yd
  let hj=z*xd-x*zd
  let hk=x*yd-y*xd
  let h0=hi*hi+hj*hj+hk*hk
  let v2=(v20-k_mu/r)
  let ei=rmu1*(v2*x-rv*xd)
  let ej=rmu1*(v2*y-rv*yd)
  let ek=rmu1*(v2*z-rv*zd)
  let ec0=ei*ei+ej*ej+ek*ek
  let sma=h0*rmu1/(1-ec0)
  let ec=sqrt ec0
  let h=sqrt h0
  let vn0=hj*hj+hi*hi + hk*hk
  let vn=sqrt vn0
  let ai=acos (hk/h)
  let anl0 = if vn /= 0 then
                acos (-hj/vn)
             else 0
  let anl = if hi<=0.0 then
                pi-anl
            else anl0
  let apg0 = if (vn*ec) /= 0 then
                (-ei*hj+ej*hi)/(vn*ec)
              else 0
  let apg1 = acos apg0
  let apg = if ek<=0.0 then
                pi-apg1
             else apg1
  let tra0 =(ei*x+ej*y+ek*z)/(ec*r)
  let tra1 = acos(tra0)
  let tra = if rv<=0.0 then
                pi-tra1
             else
                tra1
  let eca0=acos((ec+cos(tra))/(1.0+ec*cos(tra)))
  let eca = if(tra>=pi/2.0) then
                pi-eca0
             else eca0
  let am=eca-ec*sin(eca)
  OrbitalElems sma ec (atan2 ej ei) am (ai/=0)

badt :: Double
badt = 7294

bp = (-6556995.342903, 7814.932739)
bv =  (13.971285, 7814.921637)

boe = toOrbitalElements bp bv
boest@(bv',bp') = toOrbitalState boe 0
boe2 = toOrbitalElements bv' bp'

boepts = getPts boe (5 :: Double) (orbitalPeriod (oe_a boe) / 4)



