module SatM where

import qualified Interpreter as I
import qualified Port as P
import Control.Monad.State
import Control.Monad.ST
import Util
import Orbits
import Instructions
import Simulation
import Math
import qualified Data.IntMap as IM
import BurnTrace (BurnTrace)


data InterpState s = InterpState {prog :: I.CompiledBin s, state :: I.OrbitStateS s,
                                  config :: Double, step :: Int,
                                  burns :: BurnTrace}

--type StateInterpST s a= MS.StateT (InterpState s) (ST s) a
--newtype SatM s a = SatM {runSatM :: StateInterpST s a}

--instance Monad (SatM s) where
--    a >>= b = SatM $ runSatM a >>= \av -> runSatM $ b av
--    return v = SatM (return v)

--get :: SatM s (InterpState s)
--get = SatM MS.get
--put :: (InterpState s) -> SatM s ()
--put a = SatM $ MS.put a
--lift = SatM . MS.lift

type SatM s a= StateT (InterpState s) (ST s) a

-- Running

runSat :: SimBinary -> Double -> SatM s a -> ST s (a, BurnTrace)
runSat (ops, d) cfg s = do
  initS <- I.setupS ops d
  let st = InterpState (I.quasiCompile ops) initS cfg 0 []
  (a, st') <- runStateT (do idle 1; s) st
  return (a, reverse $ burns st')

-- Read these ports to get target x,y
type Body = (Addr, Addr)

self :: Body
self = (P.sxPort, P.syPort)

bodyPos :: Body -> SatM s Position
bodyPos (ax,ay) = do
  p <- getPorts
  return $ (-P.readD0 ax p, -P.readD0 ay p)

getPorts :: SatM s P.Port
getPorts = do
  p <- get
  return $ I.outPortS $ state p

getProg :: SatM s (I.CompiledBin s)
getProg = do
  p <- get
  return $ prog p

getState :: SatM s (I.OrbitStateS s)
getState = do
  p <- get
  return $ state p

getTime ::  SatM s Int
getTime = do
  p <- get
  return $ step p

makeInput :: P.Port -> SatM s P.Port
makeInput prt = do
  p <- get
  return $ P.insert P.configPort (config p) prt

updateState :: I.OrbitStateS s -> SatM s ()
updateState s' = do
  q <- get
  put $ q {state = s'}

-- These two (idle and burn) are the only ways to advance time
idle :: Int -> SatM s ()
idle 0 = return ()
idle i = do
  q@InterpState {prog=p, state=s, step=n} <- get
  inp <- makeInput P.inert
  s' <- lift $ loopM i p (s {I.inPortS = inp})
  put $ q {step=n+i, state=s'}
  return ()

burn :: Velocity -> SatM s ()
burn v = do
  q@InterpState {prog=p, state=s, burns=b, step=n} <- get
  let b' = (n,v):b
  i <- makeInput $ P.burn v
  s' <- lift $ p (s {I.inPortS = i})
  put $ q {state=s', burns=b', step=n+1}
  return ()

-- Ask the future a question (expensive though)
hypothetical :: SatM s a -> SatM s a
hypothetical test = do
  q <- get
  let s = state q
  s' <- lift $ I.copyStateS s
  r <- test
  put $ q {state=s'}
  return r

-- Expensive
getStats :: [Body] -> SatM s [(Position, Velocity)]
getStats bodies = do
  curPos <- mapM bodyPos bodies
  nextPos <- hypothetical $ do
                    idle 1
                    ps <- mapM bodyPos bodies
                    return ps
  return $ zip curPos (zipWith inferVel curPos nextPos)

---------------------------------------------------------------------
-- Circular Orbit Transfers

getTargRad :: SatM s Double
getTargRad = do
  pts <- getPorts
  return $ P.readD0 4 pts

-- Does a hohmann transfer
hohmannTrans :: Double -> SatM s ()
hohmannTrans targ = do
  [(mypos, myvel)] <- getStats [self]
  let cw = clockwiseV mypos myvel
  let (v1, v2, time) = hohmannV mypos cw targ
  burn v1
  idle (round time)
  burn v2

justAHohmann :: SatM s ()
justAHohmann = do
  targ <- getTargRad
  hohmannTrans targ


jumpOrbit :: Double -> SatM s ()
jumpOrbit targ = do
  [(mypos, myvel)] <- getStats [self]
  let myRad = vecMag mypos
  let desc = myRad > targ
  let bmag = (myRad - targ) / 2
  let bvec = bmag `pMul` normVect mypos
  burn bvec
  doUntil $ do
    idle 1
    mypos' <- bodyPos self
    let myRad' = vecMag mypos'
    if abs (myRad' - targ) < 1 ||
       desc && myRad' - targ < 1 ||
       not desc && myRad' - targ > 1
     then return True
     else return False
  mypos' <- bodyPos self
  if abs (vecMag mypos' - targ) > 1
   then jumpOrbit targ
   else stabilizeCircularOrbit

changeVelTo :: Velocity -> SatM s ()
changeVelTo v = do
  [(mypos, myvel)] <- getStats [self]
  burn $ v `pSub` myvel

stabilizeCircularOrbit :: SatM s ()
stabilizeCircularOrbit = do
  [(mypos, myvel)] <- getStats [self]
  let myRad = vecMag mypos
  let cw = clockwiseV mypos myvel
  let idealV = (visVivaCirc myRad) `pMul` (normVect . perpVect cw $ mypos)
  changeVelTo idealV

circularTransfer :: SatM s ()
circularTransfer = do
  targ <- getTargRad
  mypos <- bodyPos self
  let mymag = vecMag mypos
  if abs (mymag - targ) < 5000
   then jumpOrbit targ
   else hohmannTrans targ
  mypos' <- bodyPos self
  if abs (vecMag mypos' - targ) > 1
   then circularTransfer
   else return ()


engineTest :: SatM s ()
engineTest = do
  [(mypos, myvel)] <- getStats [self]
  burn myvel
  idle 50
  t <- getTime
  if t > 5000 then return() else engineTest
