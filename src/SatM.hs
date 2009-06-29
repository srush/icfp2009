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
import Debug.Trace


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

type SatM s a = StateT (InterpState s) (ST s) a

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
bodyPos b@(ax,ay) = do
  p <- getPorts
  return $ P.readSat p b

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

getSelfStats :: SatM s (Position, Velocity)
getSelfStats = do
  [(p,v)] <- getStats [self]
  return (p,v)

---------------------------------------------------------------------
-- Circular Orbit Transfers

getTargRad :: SatM s Double
getTargRad = do
  pts <- getPorts
  return $ P.readD0 4 pts

-- Does a hohmann transfer
hohmannTrans :: Double -> SatM s ()
hohmannTrans targ = do
  (mypos, myvel) <- getSelfStats
  let cw = clockwiseV mypos myvel
  let (v1, v2, time) = traceShow (mypos,cw,targ) $ hohmannV mypos cw targ
  burn v1
  idle (round time)
  burn v2

justAHohmann :: SatM s ()
justAHohmann = do
  targ <- getTargRad
  hohmannTrans targ


jumpOrbit :: Double -> SatM s ()
jumpOrbit targ = do
  (mypos, myvel) <- getSelfStats
  let myrad = vecMag mypos
  let targPos = targ `pMul` normVect mypos
  let cw = clockwiseV mypos myvel
  let visviva = visVivaCirc myrad
  let idealV = visviva `pMul` (normVect . perpVect cw $ mypos)
  let (shallowerPos, idealV') = integratePos targPos idealV 0
  intercept shallowerPos idealV' 2000

intercept :: Position -> Velocity -> Double -> SatM s ()
intercept p v t = do
  (mypos, myvel) <- getSelfStats
  let tv = (p `pAdd` (t `pMul` v) `pSub` mypos) `pDiv` t
  burn $ myvel `pSub` tv
  idle $ round (t - 2)
  burn $ tv `pSub` v


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
  let mymag' = vecMag mypos'
  if abs (mymag' - targ) < 5000
   then jumpOrbit targ
   else hohmannTrans targ
  return ()
--  if abs (vecMag mypos' - targ) > 1
--   then circularTransfer
--   else return ()


engineTest :: SatM s ()
engineTest = do
  (mypos, myvel) <- getSelfStats
  burn myvel
  idle 50
  t <- getTime
  if t > 5000 then return() else engineTest

rendezvous :: Position -> SatM s ()
rendezvous targ = do
  (mypos, myvel) <- getSelfStats
  let pi2 = 2 * pi
  let src = vecMag mypos
  let dst = vecMag targ
  let arc = angBetweenVects targ mypos
  let atrans = (src + dst) / 2.0
  let dttutrans  = pi * sqrt (atrans * atrans * atrans / k_mu)
  let angvelint  = sqrt (k_mu / src^3)
  let angveltgt  = sqrt (k_mu / dst^3)
  let leadang  = angveltgt * dttutrans
  let phasef = leadang - pi
  let waittime = abs $ (phasef - arc) / (angvelint - angveltgt)
  let sme1 = -k_mu / (2.0 * src)
  let sme2 = -k_mu / (2.0 * atrans)
  let sme3 = -k_mu / (2.0 * dst)
  let  vinit    = sqrt(2.0 * ((k_mu / src) + sme1))
  let  vtransa  = sqrt(2.0 * ((k_mu / src) + sme2))
  let  deltava  = abs(vtransa - vinit)
  let  vfinal   = sqrt(2.0 * ((k_mu / dst) + sme3))
  let  vtransb  = sqrt(2.0 * ((k_mu / dst) + sme2))
  let  deltavb  = abs(vfinal - vtransb)
  let  k = if (src < dst) then 1 else -1
  idle $ round waittime
  let motdir = normVect myvel
  burn $ k * deltava `pMul` motdir
  idle $ round dttutrans
  burn $ k * deltavb `pMul` motdir

