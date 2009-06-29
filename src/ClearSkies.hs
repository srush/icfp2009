{-# LANGUAGE DeriveDataTypeable, ExistentialQuantification, TemplateHaskell #-}
module ClearSkies where
import Data.Array.IO
import Data.Array
import Math
import Orbits
import BurnTrace
import Instructions
import Visualizer
import System.Environment
import qualified OpParser as OP
import Control.Applicative
import Control.Monad
import Data.Binary
import Data.DeriveTH
import Data.Derive.Eq
import Data.Derive.Binary
import qualified Port as P
import Util

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

emptyClearSkies = ClearSkies 0 0 0 (0.0,0.0) (0,0) []

toClearSkies arr = ClearSkies {
  myscore = arr ! 0,
  myfuel = arr ! 1,
  sat = mypos,
  refuelPos = (arr ! 4, arr ! 5), 
  stationFuel = (arr ! 6),
  targets = map (toTarget arr mypos) $ map (\x -> (x*3) +7) [0..10]
}
    where mypos = (arr ! 2, arr ! 3)

n_sats = 12

sats :: [(Addr, Addr)]
sats = map (\i -> (3 * i + 7, 3 * i + 8)) [0..n_sats-1]

portToClearSkies :: P.Port -> ClearSkies
portToClearSkies p =
    let rp a = P.readD0 a p
    in
      ClearSkies {
      myscore = rp 0,
      myfuel = rp 1,
      sat = P.readSat p (2, 3),
      refuelPos = P.readSat p (4, 5),
      stationFuel = (rp 6),
      targets = map (portToTarget p) $ map (\x -> (x*3) +7) [0..10]
    }

portToTarget p start = Target {
                         tarpos = P.readSat p (start, start+1),
                         collected = 1 == P.readD0 (start+2) p}

toTarget arr (x,y) start = Target {
  tarpos  = (x - (arr ! start),  y - (arr ! (start + 1))),
  collected = ((arr ! (start + 2)) == 1.0)
}



satsDrawers :: [Drawer]
satsDrawers = map (\p pm pc -> satDrawer pm pc p c_green) sats

withDataCollector :: Int -> (BurnTraceCallback -> IO ()) -> IO (Array Int ClearSkies)
withDataCollector n runit = do
  a <- newTrashCan
  runit $ \p -> do
    putItInTheTrash a (portToClearSkies p)
  t <- emptyTheTrash a
  return $ listArray (0, length t - 1) t


--main = do
--  [cfgS, stepsS] <- getArgs
--  let cfg = (read cfgS)
--  bin <- OP.readBin "../bins/bin4.obf"
--  v <- runWithVisualization sats $ runBurnTrace bin cfg [] (read stepsS)
--  commence v
$( derive makeBinary ''Target )
$( derive makeBinary ''ClearSkies )

main = do
  [cfgS, stepsS] <- getArgs
  let cfg = (read cfgS)
  bin <- OP.readBin "../bins/bin4.obf"
  let steps = read stepsS
  dat <- withDataCollector steps $  runBurnTrace bin cfg [] steps
  print $ dat ! 0
--  v <- runWithVisualization satsDrawers defaultOps $
--       runBurnTrace bin cfg [] (read stepsS)
--  commence v

