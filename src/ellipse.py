from math import sqrt, sin, cos
import math

p2001 = [-6556995.342902722,
          7814.932738513376,
          -6556981.371617502,
          15629.854375943332,
          1800001.7901158966,
          892.5973799914409,
          1800007.1604589382,
          1785.1884085001784]

p2002 = [
    -6352278.9303242555,
     6361717.5661423905,
     -6347554.359717081,
     6366431.626151435,
     5.721747189965483e7,
     2226.3589487336576,
     5.722194720261464e7,
     4449.221530630253]


p3001 =  [
    -6556995.342902722,
     7814.932738513376,
     -6556981.371617502,
     15629.854375943332,
     1800001.7901158966,
     200.3638441392468,
     1800007.1604601778,
     400.7218117547327
]


p3002 =  [
 -6352278.9303242555,
 6361717.5661423905,
 -6347554.359717081,
 6366431.626151435,
 5.721749681177557e7,
 2475.480156233534,
 5.7221997026856095e7,
 4947.463945247233
]

p3002c =  [
 0,0,0,0,
 0,0, 1000, 0
]



p3003 =[-8356997.133018618,
6922.335358521935,
-8356988.53207644,
13844.665967443154,
-1000000.8323579067,
-3406.598441917172,
-1000003.3294191593,
6813.191246001246]


p3004 =  [
    -7875.215433235455,
     -6456995.197536153,
     -15750.419151916967,
     -6456980.790151756,
     -260.646538861326,
     1900001.9354824657,
     -521.2865877283675,
     1900007.7419259232
]

(x0,y0,x1,y1,tdx0,tdy0,tdx1,tdy1) = p2001

G = 6.67428e-11
M = 6.0e24
mu = G*M

def dock(t):
  rx0 = x0 - tdx0
  ry0 = y0 - tdy0
  rx1 = x1 - tdx1
  ry1 = y1 - tdy1
  rvx = rx1 - rx0
  rvy = ry1 - ry0

  tx = tdx0 - x0
  ty = tdy0 - y0
  cp = (tx*x0 + ty*y0)
  if cp != 0:
    cp /= sqrt(tx**2 + ty**2)
    cp /= sqrt(x0**2 + y0**2)
  w = math.acos(cp)

  print tx, ty, cp
  print w
  delta = 3*w*t*sin(w*t) - 8*(1 - cos(w*t))

  xr = ((w / delta) * (rx0 * sin(w*t) +
       ry0 * (6*w*t*sin(w*t) - 14*(1-cos(w*t)))))
  yr = ((w / delta) * 2*rx0*(1 - cos(w*t)) +
       ry0*(4*sin(w*t) - 3*w*t*cos(w*t)))
  delta_v1 = ((xr - rvx) **2 + (yr - rvx)**2) ** 0.5
  return [delta_v1]

def p():
  tx0 = tdx0  - x0
  ty0 = tdy0 - y0


  tx1 = tdx1 - x1
  ty1 = tdy1 - y1

  vx = x1 - x0
  tvx = tx1 - tx0

  vy = y1 - y0
  tvy = ty1 - ty0

  print 's: (%f, %f)  (%f, %f)' % (x0, y0, vx, vy)
  o(x0, y0, vx, vy)
  print '----------------'
  print 't: (%f, %f)  (%f, %f)' % (tx0, ty0, tvx, tvy)
  o(tx0, ty0, tvx, tvy)

  print 'phase angle: %f'%(math.atan2(y0, x0) - math.atan2(ty0, tx0))

def o(x, y, xd, yd):
  z=0
  zd=0;
  rmu1=1.0/mu
  v2 = xd*xd+yd*yd+zd*zd
  r=x*x+y*y+z*z;
  r=sqrt(r); rv=x*xd+y*yd+z*zd

  hi=y*zd-z*yd;hj=z*xd-x*zd;hk=x*yd-y*xd;
  h=hi*hi+hj*hj+hk*hk;v2=(v2-mu/r)

  ei=rmu1*(v2*x-rv*xd);ej=rmu1*(v2*y-rv*yd);ek=rmu1*(v2*z-rv*zd)
  ec=ei*ei+ej*ej+ek*ek;
  sma=h*rmu1/(1-ec);ec=sqrt(ec);h=sqrt(h)
  vn=hj*hj+hi*hi + hk*hk; vn=sqrt(vn);
  ai=math.acos(hk/h);

  anl = 0
  if vn != 0:
    anl=math.acos(-hj/vn)
  if(hi<=0.0):
    anl=math.pi-anl;
  apg = 0

  if (vn*ec) != 0:
    apg=(-ei*hj+ej*hi)/(vn*ec);

  apg=math.acos(apg);
  if(ek<=0.0):
    apg=math.pi-apg;
  tra=(ei*x+ej*y+ek*z)/(ec*r);
  tra=math.acos(tra);
  if(rv<=0.0):
    tra=math.pi-tra;
  eca=math.acos((ec+math.cos(tra))/(1.0+ec*math.cos(tra)))
  if(tra>=math.pi/2.0):
    eca=math.pi-eca;
  am=eca-ec*math.sin(eca)

  print 'semi-major:   %f' % sma;
  print 'eccentricity: %f' % ec;
  print 'inclination:  %f' % (ai*180.0/math.pi)
  print 'angle perigee: %f' % (math.atan2(ej,ei))
  print 'arc perigee:  %f' % (apg*180.0/math.pi)
  print 'right anmaly: %f' % (anl*180.0/math.pi)
  print 'mean anomaly: %f' % (am*180.0/math.pi)
  print 'true anomaly: %f' % (tra*180.0/math.pi)
  print 'eccentric an: %f' % (eca*180.0/math.pi)


print o(4e8,0,0,1000.57)
print o(-6556995.342903, 7814.932739,13.971285, 7814.921637)
