module BurnTrace where

import Orbits
import Visualizer
import Graphics.UI.Gtk
import Interpreter
import Control.Monad.ST
import Instructions
import qualified Port as P
import Util

type BurnTrace = [(Int, Velocity)]

type BurnTraceCallback = P.Port -> IO ()

commence :: (BurnTraceCallback -> IO ()) -> IO ()
commence b = b nullCallback

nullCallback :: BurnTraceCallback
nullCallback _ = return ()

addCallbacks :: BurnTraceCallback -> BurnTraceCallback -> BurnTraceCallback
addCallbacks f g = \p -> do
                     f p
                     g p

runBurnTrace :: SimBinary -> Double -> BurnTrace -> Int -> BurnTraceCallback -> IO ()
runBurnTrace bin cfg bt steps pcbs = do
  (prog, state) <- stToIO $ compileSimBinary bin
  state' <- stToIO $ initSim prog state cfg
  loop prog state' bt 0
 where
   progBurn prog s v = stToIO $ prog s {inPortS=P.setBurn v (inPortS s)}
   progSleep prog s = stToIO $ prog s {inPortS=P.setInert (inPortS s)}
   loop prog s b n | n == steps = return ()
                   | otherwise = do
     pcbs (outPortS s)
     case b of
       ((i,v):t) ->
           if i == n 
            then do
             s' <- progBurn prog s v
             loop prog s' t (n+1)
            else do
             s' <- progSleep prog s
             loop prog s' b (n+1)
       [] -> do
              s' <- progSleep prog s
              loop prog s' [] (n+1)
                     

runWithVisualization :: (BurnTraceCallback -> IO ()) -> IO (BurnTraceCallback -> IO ())
runWithVisualization runit = do
  initGUI
  pm <- initPixmap
  canvas <- drawingAreaNew
  pc <- widgetGetPangoContext canvas
  drawer <- everyN 20 (drawPortVals pm pc)
  return $ \cb -> do
      runit (cb `addCallbacks` drawer)
      displayPm pm canvas

