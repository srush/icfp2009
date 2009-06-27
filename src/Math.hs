module Math where

normAng :: Double -> Double
normAng a | a < -pi = normAng (a + 2 * pi)
          | a > pi = normAng (a - 2 * pi)
          | otherwise = a
