
module Battins where
import Data.Complex
import Data.Fixed
--import Debug.Trace
import Data.List

_E (x,shortPath) = 2 * atan((if shortPath then (1) else (1))* x**0.5)

cubic_soln :: (Double,Double,Double) -> Double
cubic_soln (a, b, c) =
    if p == 0.0 then
        if q > 0 then
            (-q**(1 /3) - a/3.0)
        else
            ((-q)**(1/3) - a/3.0)
    else
        maximum $ map realPart $ possible_solutions u v 3
    where
      p =  (b - a**2/3.0)
      q = c + (2* a**3 - 9 * a * b) / 27.0
    -- special case

      u_cubed :: Complex Double
      u_cubed =  ((-q)/2 :+ 0) + (((q**2 / 4 + p^3 / 27)) :+ 0) **0.5
      u :: Complex Double
      u = u_cubed**(1/3)
      v = -(p:+0) / u / 3.0
      rotation = (-0.5) :+ (1*3**0.5/2)

      possible_solutions :: Complex Double -> Complex Double -> Int -> [Complex Double]
      possible_solutions u v 0 = []
      possible_solutions u v n =
          (-(p:+0) / 3.0 / u + u - (a:+0)/3.0) : possible_solutions (u * rotation) (v / rotation) (n -1)

special_cubic_soln' :: Double -> Double
special_cubic_soln' c =
    maximum $ map realPart  $ possible_solutions u v 3
    where
  p = -1/3.0
  q = c - 2.0/27.0
  u_cubed = (- q/2 + (q**2 / 4 + p**3 / 27)**0.5) :+ 0
  u = u_cubed**(1/3)
  v = -(p:+0) / u / 3
  rotation = ((-0.5) +(3**0.5/2)) :+ 1
  possible_solutions :: Complex Double -> Complex Double -> Int -> [Complex Double]
  possible_solutions u v 0 = []
  possible_solutions u v n =
   (-(p:+0) / 3 / u + u + 1/3.0) : possible_solutions (u * rotation) (v / rotation) (n -1)

special_cubic_soln :: Double -> Double
special_cubic_soln c =
    max_possible_solution (-1/0) u v 3
    where
  p = -1/3.0
  q = c - 2.0/27.0
  u_cubed = (- q/2 + (q**2 / 4 + p**3 / 27)**0.5) :+ 0
  u = u_cubed**(1/3)
  v = -(p:+0) / u / 3
  rotation = ((-0.5) +(3**0.5/2)) :+ 1
  max_possible_solution :: Double -> Complex Double -> Complex Double -> Int -> Double
  max_possible_solution m u v 0 = m
  max_possible_solution m u v n = max_possible_solution m' (u * rotation) (v / rotation) (n -1)
      where
        s = realPart (-(p:+0) / 3 / u + u + 1/3.0)
        m' = max s m


_y(m, e) =  special_cubic_soln c
    where
  a = -1
  b = 0
  c = - m * (e - sin(e)) / (4 * tan(e/2)**3)


_x(m, ell, y) = (-(ell + 1) + ((ell + 1)**2 - 4 * (ell - m/y**2))**0.5)/2

next_x(m, ell, x, shortPath) = _x(m, ell, y)
    where
      e = _E(x, shortPath)
      y = _y(m, e)


battins_inner(m, ell, precision, x_0, shortPath) = (new_x, _y(m, _E(new_x, shortPath)))
    where
  old_x = x_0
  new_x' = next_x(m, ell, old_x, shortPath)
  new_x = loop old_x new_x'
  loop old_x new_x =
      if abs(new_x - old_x) > precision then
          loop new_x  $ next_x(m, ell, new_x, shortPath)
      else new_x


_r_0_p(nu, d) =  d * (cos(nu) + 1) / 4 / sin(nu)


sq_dist((p1x,p1y), (p2x, p2y)) = (p1x - p2x)**2 + (p1y - p2y)**2


mu = 400456.8

battins (p1@(p1x, p1y), p2@(p2x, p2y), t_f, shortPath) =
    if a > 0 then
        Just (v1, v2)
    else Nothing
    where
  r_1 = sq_dist(p1, (0, 0))**0.5
  r_2 = sq_dist(p2, (0, 0))**0.5
  d = sq_dist(p1, p2)**0.5
  theta_1 = acos(p1x / r_1) *
            (if p1y < 0 then -1
            else 1)
  theta_2 :: Double
  theta_2 = acos(p2x / r_2) *
            (if p2y < 0 then -1 else 1)
  theta' = (theta_2 - theta_1) `mod'` (2*pi)

  theta'' =
      (if theta' > pi then theta' - 2*pi
      else theta')

  theta = if not shortPath then
              2*pi - theta''
          else theta''



  c = (r_1**2 + r_2**2 - 2.0 * r_1 * r_2 * cos(theta))**0.5
  eta = (r_1 + r_2) / 2.0
  nu = acos((r_1*r_2)**0.5 * cos(theta/2.0) / eta)
  r_0_p = _r_0_p(nu, d)
  ell = tan(nu / 2.0)**2
  m = mu * t_f**2 / 8 / r_0_p**3
  (x, y) = battins_inner(m, ell, 0.00000001, 1.0, shortPath)

  s = (d + (d**2 + 4*r_1*r_2*cos(theta/2)**2)**0.5) / 2

  a = mu * t_f**2 / 16 / r_0_p**2 / x / (y**2)
  arg1 = (s / 2.0 / a)**0.5
  arg2 = ((s - c) / 2.0 / a)**0.5
  sinv = arg2
  bete = if (theta < pi) then 2.0*asin(sinv) else -2.0*asin(sinv)

  cosv= sqrt( 1.0 - s/(2.0*a) )
  sinv'= arg1

  am  = s*0.5
  be  = 2.0*asin( sqrt( (s-c)/s ) )

  tm  = sqrt(am*am*am/mu)*(- (be-sin(be)))

  alpe =
      if  t_f > tm  then
          2.0*pi-2.0*asin( sinv' )
      else
          2.0*asin( sinv' )
  de  = alpe - bete
  f   = 1.0 - (a/r_1)*(1.0 - cos(de) )
  gdot= 1.0 - (a/r_2)* (1.0 - cos(de) )
  g   = t_f - sqrt(a*a*a/mu)*(de - sin(de))


  v1 = ((p2x - f*p1x) / g, (p2y - f*p1y) / g)
  v2 = ((gdot * p2x - p1x) / g, (gdot * p2y - p1y) / g)

