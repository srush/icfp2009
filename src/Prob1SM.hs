module Prob1SM where

import SatM
import qualified OpParser as OP
import System.Environment
import Control.Monad.ST
import BurnTrace

main :: IO ()
main = do
  [cfgS, traceS] <- getArgs
  bin <- OP.readBin "../bins/bin1.obf"
  let cfg = (read cfgS)
  let (_, burns) = runST $ runSat bin cfg justAHohmann
  print $ take 50 burns
  let bt = runBurnTrace bin cfg burns 100000
  v <- runWithVisualization bt
  commence v