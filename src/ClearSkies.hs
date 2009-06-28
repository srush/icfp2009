module ClearSkies where
import Data.Array
import Math
import Orbits
import BurnTrace
import Instructions
import Visualizer
import System.Environment
import qualified OpParser as OP

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
  sat = (arr ! 2, arr ! 3),
  refuelPos = (arr ! 4, arr ! 5),
  stationFuel = (arr ! 6),
  targets = map (toTarget arr) $ map (\x -> (x*3) +7) [0..10]
}


toTarget arr start = Target {
  tarpos  = (arr ! start, arr ! (start + 1)),
  collected = ((arr ! (start + 2)) == 1.0)
}

n_sats = 12

sats :: [Drawer]
sats = take 1 $ map (\i pm pc -> satDrawer pm pc (3 * i + 7, 3 * i + 8) c_green) [0..n_sats-1]


main = do
  [cfgS, stepsS] <- getArgs
  let cfg = (read cfgS)
  bin <- OP.readBin "../bins/bin4.obf"
  v <- runWithVisualization sats $ runBurnTrace bin cfg [] (read stepsS)
  commence v
