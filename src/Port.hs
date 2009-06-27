module Port where

import Instructions
import qualified Data.Map as M

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

configPort :: Addr
configPort = 16000

readScore :: Port -> Double
readScore p = findWithDefault 0.0 scorePort p

setConfig :: Double -> Port -> Port
setConfig v p = M.insert configPort v p
