module Port where

import Instructions
import qualified Data.Map as M
import qualified Data.List as L
import Math

type Port = M.Map Addr Double

findWithDefault :: Double -> Addr -> Port -> Double
findWithDefault = M.findWithDefault
singleton :: Addr -> Double -> Port
singleton = M.singleton
empty :: Port
empty = M.empty
insert :: Addr -> Double -> Port -> Port
insert = M.insert
fromList :: [(Addr, Double)] -> Port
fromList = M.fromList
toList :: Port -> [(Addr, Double)]
toList = M.toList
size :: Port -> Int
size = M.size


scorePort :: Addr
scorePort = 0

fuelPort :: Addr
fuelPort = 1

sxPort :: Addr
sxPort = 2

syPort :: Addr
syPort = 3

dVxPort :: Addr
dVxPort = 2

dVyPort :: Addr
dVyPort = 3

configPort :: Addr
configPort = 16000


readD0 :: Addr -> Port -> Double
readD0 = findWithDefault 0.0

readScore :: Port -> Double
readScore = readD0 scorePort

readFuel :: Port -> Double
readFuel = readD0 fuelPort

readSX :: Port -> Double
readSX = readD0 sxPort

readSY :: Port -> Double
readSY = readD0 syPort

readS :: Port -> (Double, Double)
readS p = (readSX p, readSY p)

readSat :: Port -> (Addr, Addr) -> (Double, Double)
readSat p (a1, a2) = if a1 == sxPort
                      then s
                      else s `pSub` (readD0 a1 p, readD0 a2 p)
  where
    s = readS p

setConfig :: Double -> Port -> Port
setConfig v p = M.insert configPort v p

readStd :: Port -> (Double, Double, Double, Double)
readStd p = (readFuel p, readSX p, readSY p, readScore p)



inert :: Port
inert = burn (0, 0)

setInert :: Port -> Port
setInert = setBurn (0,0)

burn :: (Double, Double) -> Port
burn (dx, dy) = M.insert dVxPort dx (M.singleton dVyPort dy)

setBurn :: (Double, Double) -> Port -> Port
setBurn (dx, dy) p = M.insert dVxPort dx (M.insert dVyPort dy p)

csvPort :: Port -> String
csvPort p = L.intercalate "," . map (show.snd) . M.toList $ p

csvPorts :: [Port] -> String
csvPorts ports = unlines (map csvPort ports)
