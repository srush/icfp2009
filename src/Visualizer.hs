module Visualizer where

import Graphics.UI.Gtk.Glade
import Graphics.UI.Gtk
import Graphics.UI.Gtk.General.General
import Client
import qualified Port as P
import Orbits
import qualified Control.Monad.State as MS
import Control.Monad.Trans (liftIO)
import Math
import Instructions
import BurnTrace

data VisOpts = VisOpts {scaleFactor :: Double, winSize :: Int}

defaultOps :: VisOpts
defaultOps = VisOpts {scaleFactor=100000, winSize = 1024}

type VisOptions a = MS.StateT VisOpts IO a

earthPos :: Int
earthPos = 512

center :: (Int, Int)
center = (earthPos, earthPos)

earthSize :: VisOptions Int
earthSize = scale radius_e

scale :: Double -> VisOptions Int
scale v = do
  q <- MS.get
  return $ round (v / (scaleFactor q))

scaleV :: (Double, Double) -> VisOptions (Int, Int)
scaleV (x,y) = do
  q <- MS.get
  x' <- scale x
  y' <- scale y
  return (x',y')

c_white :: Color
c_white =  Color 65535 65535 65535
c_black :: Color
c_black = Color 0 0 0
c_red :: Color
c_red = Color 65535 0 0
c_green :: Color
c_green = Color 0 65535 0

-- Scale factor is how many m per pixel
coordToPixel :: (Double, Double) -> VisOptions (Int, Int)
coordToPixel pos = do
  (x,y) <- scaleV pos
  return (x + earthPos, -y + earthPos)

drawCircle :: Pixmap -> GC -> Bool -> (Int, Int) -> Int -> VisOptions ()
drawCircle pm gc fill (x,y) width = liftIO $ drawArc pm gc fill x' y' width width 0 23040
    where
      (x',y') = (x - width `div` 2, y - width `div` 2)

satDrawer :: Pixmap -> PangoContext -> (Addr, Addr) -> Color ->  P.Port ->
             VisOptions ()
satDrawer pm _ (xa, ya) c prt = _satDrawer pm (xa, ya) c prt

_satDrawer :: Pixmap -> (Addr, Addr) -> Color ->  P.Port -> VisOptions ()
_satDrawer pm (xa, ya) clr prt = do
  let s = P.readSat prt (xa, ya)
  q <- MS.get
  c <- coordToPixel s
  gc <- liftIO $ gcNewWithValues pm (newGCValues {foreground = clr})
  drawCircle pm gc True c 3

drawPortVals :: Pixmap -> PangoContext -> P.Port -> VisOptions ()
drawPortVals pm pc prt = do
  satDrawer pm pc (P.sxPort, P.syPort) c_red prt

textDrawer :: (P.Port -> String) -> Drawer
textDrawer ps pm pc prt = do
  let scs = ps prt
  gc <- liftIO $ gcNewWithValues pm (newGCValues {foreground = c_white})
  pl <- liftIO $ layoutText pc scs
  (Rectangle x y w h , _) <- liftIO $ layoutGetPixelExtents pl
  gcb <- liftIO $ gcNewWithValues pm (newGCValues {foreground = c_black})
  liftIO $ drawRectangle pm gcb True x y w h
  liftIO $ drawLayout pm gc 0 0 pl

blankPm :: VisOptions Pixmap
blankPm = do
  q <- MS.get
  let ws = winSize q
  p <- liftIO $ pixmapNew (Nothing :: Maybe Pixmap) ws ws (Just 24)
  gc <- liftIO $ gcNewWithValues p (newGCValues {foreground = c_black})
  liftIO $ drawRectangle p gc True 0 0 ws ws
  return p

initPixmap :: VisOptions Pixmap
initPixmap = do
  pm <- blankPm
  gc <- liftIO $ gcNewWithValues pm (newGCValues {foreground = Color 0 0 65535})
  es <- earthSize
  drawCircle pm gc True center es
  return pm

type Drawer = Pixmap -> PangoContext -> P.Port -> VisOptions ()

--clientDrawAdapter :: Pixmap -> [Drawer] -> PangoContext -> Client -> Client
--clientDrawAdapter pm cbs pc cli p = do
--  drawPortVals pm pc p
--  mapM_ (\c -> c pm pc p) cbs
--  cli p

radiusDrawer :: Addr -> Color -> Drawer
radiusDrawer addr clr pm _ prt = do
  r <- scale (P.readD0 addr prt)
  gc <- liftIO $ gcNewWithValues pm (newGCValues {foreground = clr})
  drawCircle pm gc False center (2*r)

withVisClient ::[Drawer] -> Client -> (Client -> IO Double) -> IO Double
withVisClient cbs cli cb = do
  initGUI
  return 1.0
 -- pm <- initPixmap
 -- canvas <- drawingAreaNew
 -- pc <- widgetGetPangoContext canvas
 -- ad <- adapterSkipper 100 (clientDrawAdapter pm cbs pc)
 -- s <- cb $ ad cli
 -- displayPm pm canvas
 -- return s

displayPm :: Pixmap -> DrawingArea -> VisOptions ()
displayPm pm canvas = do
  q <- MS.get
  let ws = winSize q
  window <- liftIO windowNew
  liftIO $ onDelete window (\_ -> return False)
  liftIO $ onDestroy window mainQuit
  liftIO $ set window [windowDefaultWidth := ws+1, windowDefaultHeight := ws+1,
                       containerBorderWidth := 1, containerChild := canvas]
  gc <- liftIO $ gcNewWithValues pm newGCValues
  liftIO $ onExpose canvas (\_ -> do
                              dw <- widgetGetDrawWindow canvas
                              drawDrawable dw gc pm 0 0 0 0 ws ws
                              return True)
  liftIO $ widgetShowAll window
  liftIO $ mainGUI


runWithVisualization :: [Drawer] -> VisOpts -> (BurnTraceCallback -> IO ()) ->
                        IO (BurnTraceCallback -> IO ())
runWithVisualization drs vops runit = evalStateT work vops
    where
      opit x p = evalStateT (x p) vops
      work = do
        liftIO $ initGUI
        pm <- initPixmap
        canvas <- liftIO $ drawingAreaNew
        pc <- liftIO $ widgetGetPangoContext canvas
        let allcbs = foldl (\bcb lcb p -> do
                              bcb p
                              (lcb pm pc p))
                     (drawPortVals pm pc) drs
        drawer <- liftIO $ everyN 60 (opit allcbs)
        return $ \cb -> do
               runit (cb `addCallbacks` drawer)
               evalStateT (displayPm pm canvas) vops
