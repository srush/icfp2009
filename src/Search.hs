
module Main where 
import Orbits
import Battins
import Math
import Data.List 
import Data.Function
import Debug.Trace
import qualified Data.Map as M
import Data.Map ((!))

type Pos = (Double, Double)  
type Vel = (Double, Double)  
type Init = (Pos, Vel)  




lambert :: Pos -> Pos -> Integer -> Maybe (Vel, Vel)
lambert (x,y) (x',y') time = battins ((x/1000, y/1000), (x'/1000,y'/10000), fromIntegral time)


maxFuel = 10000.0

travelCost :: (Integer , Integer , Init , Init) -> Double
travelCost (wait, travel, start, end) = trace ("Lambert: " ++(show res) ++ "\n Wait :" ++  (show wait) ++ "\n Travel :" ++  (show travel) ++ "\nScore: " ++ show ret) ret
   where       
     ret =     case res of 
      Just (vstart, vend) -> 
          log(fromIntegral wait + fromIntegral travel) + (45.0/ maxFuel) * (f1 + f2) + (if (f1+f2) > maxFuel then 100000000.0 else 0.0)
          where f1 = trace (show $  vcur ) $ vecMag $ (1000 `pMul` vstart) `pSub` vcur
                f2 = trace (show $  vfinish) $ vecMag $ vfinish `pSub` (1000 `pMul` vend) 
      Nothing -> 1000000000.0

     res = lambert pos endpos travel
     (vcur, pos) = start
     (vfinish, endpos) = end
      
          
waitTimes = [7284]
travelTimes = [10, 3197]


search ::  Pos -> Vel -> Pos -> Vel -> (Integer, Integer, Init, Init)
search (x, y) (vx, vy) (x', y') (vx', vy') = 
    minimumBy (compare `on` travelCost) [(t, e, atTime innerOrb t, atTime outerOrb (t+e))
                                            | t <-  waitTimes,
                                              e <-  travelTimes]    
    where
      innerOrb = toOrbitalElements (x,y) (vx,vy)
      outerOrb = toOrbitalElements (x',y') (vx', vy')
      atTime map time = toOrbitalState map $ fromIntegral time  
      

main = do
  contents <- readFile "allpoints2001"
  --let (us, them)  = read contents
  --let (usmap, themmap ) = (M.fromList us, M.fromList them)
 --  print $ usmap ! 7294 
  let s = search  (-6556995.342903, 7814.932739)  (13.971285, 7814.921637) (8356997.133019, -6922.335359)  (-8.600942, -6922.330609)
  print $ show s