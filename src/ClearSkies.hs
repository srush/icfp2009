module ClearSkies where 
import Data.Array
import Math
import Orbits

data Target = Target {
      tarpos :: Position,
      collected :: Bool
} deriving Show 

data ClearSkies = ClearSkies {
      myscore :: Double,
      myfuel :: Double,
      stationFuel :: Double,
      sat :: Position,
      refuelPos :: Position,
      targets :: [Target]
} deriving Show

toClearSkies arr = ClearSkies {
  myscore = arr ! 0,
  myfuel = arr ! 1,
  sat = mypos,
  refuelPos = (arr ! 4, arr ! 5), 
  stationFuel = (arr ! 6),
  targets = map (toTarget arr mypos) $ map (\x -> (x*3) +7) [0..10]
}
    where mypos = (arr ! 2, arr ! 3)


toTarget arr (x,y) start = Target {
  tarpos  = (x - (arr ! start),  y - (arr ! (start + 1))),
  collected = ((arr ! (start + 2)) == 1.0)
}