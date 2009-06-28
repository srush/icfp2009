module Math where

normAng :: Double -> Double
normAng a | a < -pi = normAng (a + 2 * pi)
          | a > pi = normAng (a - 2 * pi)
          | otherwise = a

normAng2Pi :: Double -> Double
normAng2Pi a | a < 0 = normAng2Pi (a + 2 * pi)
             | a > 2*pi = normAng2Pi (a - 2 * pi)
             | otherwise = a

pAdd :: Num a => (a, a) -> (a, a) -> (a, a)
pAdd (x1,y1) (x2,y2) = (x1+x2, y1+y2)

pSub :: Num a => (a,a) -> (a,a) -> (a,a)
pSub (x1,y1) (x2,y2) = (x1-x2, y1-y2)

pMul :: Num a => a -> (a, a) -> (a, a)
pMul s (x,y) = (s*x, s*y)

pDiv :: Floating a => (a,a) -> a -> (a,a)
pDiv (x,y) s = (x/s, y/s)

vecMag :: Floating a => (a, a) -> a
vecMag (x ,y) = sqrt (x^2 + y^2)

normVect :: Floating a => (a,a) -> (a,a)
normVect v = v `pDiv` (vecMag v)

negVect :: Num a => (a,a) -> (a,a)
negVect (x,y) = (-x,-y)

--Hurr
cubeRoot :: Floating a => a -> a
cubeRoot v = exp ((1/3) * log v)

perpVect :: Num a => Bool -> (a,a) -> (a,a)
perpVect cw (x,y) = if cw then (y,-x) else (-y,x)

dot :: Num a => (a,a) -> (a,a) -> a
dot (x1,y1) (x2,y2) = x1*x2 + y1*y2

k_e = exp 1

rotateVect :: Floating a => a -> (a,a) -> (a,a)
rotateVect a (x,y) = (x * cos a - y * sin a,
                      x * sin a + y * cos a)

angBetweenVects :: Floating a => (a,a) -> (a,a) -> a
angBetweenVects v1 v2 = atan (yd / xd)
    where
      (xd,yd) = v2 `pSub` v1

-- With pairs only the z will be nonzero
crossProd :: Num a => (a,a) -> (a,a) -> a
crossProd (a1,a2) (b1,b2) = a1 * b2 - a2 * b1