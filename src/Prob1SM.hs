module Prob1SM where

import SatM
import qualified OpParser as OP
import System.Environment
import Control.Monad.ST
import BurnTrace
import Visualizer
import qualified Port as P
import Math

pDrawer :: Drawer
pDrawer = textDrawer pd
    where
      pd prt = unlines $ (show self : map show [scr, fuel, tr, rdiff])
          where
            (fuel,_ ,_ ,scr) = P.readStd prt
            self = P.readS prt
            tr = P.readD0 4 prt
            rdiff = vecMag self - tr

main :: IO ()
main = do
  [cfgS, stepS] <- getArgs
  bin <- OP.readBin "../bins/bin1.obf"
  let cfg = (read cfgS)
  let (_, burns) = runST $ runSat bin cfg circularTransfer
  let bt = runBurnTrace bin cfg burns (read stepS)
  print $ take 5 burns
  v <- runWithVisualization [radiusDrawer 4 c_white, pDrawer] defaultOps bt
  commence v
