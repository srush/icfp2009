module Math where

normAng :: Double -> Double
normAng a | a < -pi = normAng (a + 2 * pi)
          | a > pi = normAng (a - 2 * pi)
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
