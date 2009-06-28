module Visualizer where

import Graphics.UI.Gtk.Glade
import Graphics.UI.Gtk
import Graphics.UI.Gtk.General.General
import Client
import qualified Port as P
import Orbits
import Math
import Instructions

winSize :: Int
winSize = 1024

earthPos :: Int
earthPos = 512

center :: (Int, Int)
center = (earthPos, earthPos)

earthSize :: Int
earthSize = scale defaultScale radius_e

defaultScale :: Double
defaultScale = 100000

scale :: Double -> Double -> Int
scale s v = round (v / s)

c_white :: Color
c_white =  Color 65535 65535 65535
c_black :: Color
c_black = Color 0 0 0
c_red :: Color
c_red = Color 65535 0 0
c_green :: Color
c_green = Color 0 65535 0

-- Scale factor is how many m per pixel
coordToPixel :: Double -> (Double, Double) -> (Int, Int)
coordToPixel s pos = ((round x) + earthPos, (round (-y)) + earthPos)
    where
      (x,y) = pos `pDiv` s

drawCircle :: Pixmap -> GC -> Bool -> (Int, Int) -> Int -> IO ()
drawCircle pm gc fill (x,y) width = drawArc pm gc fill x' y' width width 0 23040
    where
      (x',y') = (x - width `div` 2, y - width `div` 2)

satDrawer :: Pixmap -> PangoContext -> (Addr, Addr) -> Color ->  P.Port -> IO ()
satDrawer pm _ (xa, ya) c prt = _satDrawer pm (xa, ya) c prt

_satDrawer :: Pixmap -> (Addr, Addr) -> Color ->  P.Port -> IO ()
_satDrawer pm (xa, ya) clr prt = do
  let s = (-P.readD0 xa prt, -P.readD0 ya prt)
  let s' = if xa /= P.sxPort then
               let self = (-P.readSX prt, -P.readSY prt)
               in self `pAdd` s
            else s
  let c = coordToPixel defaultScale s
  gc <- gcNewWithValues pm (newGCValues {foreground = clr})
  drawCircle pm gc True c 3

drawPortVals :: Pixmap -> PangoContext -> P.Port -> IO ()
drawPortVals pm pc prt = do
  satDrawer pm pc (P.sxPort, P.syPort) c_red prt

textDrawer :: (P.Port -> String) -> Drawer
textDrawer ps pm pc prt = do
  let scs = ps prt
  gc <- gcNewWithValues pm (newGCValues {foreground = c_white})
  pl <- layoutText pc scs
  (Rectangle x y w h , _) <- layoutGetPixelExtents pl
  gcb <- gcNewWithValues pm (newGCValues {foreground = c_black})
  drawRectangle pm gcb True x y w h
  drawLayout pm gc 0 0 pl

blankPm :: IO Pixmap
blankPm = do
  p <- pixmapNew (Nothing :: Maybe Pixmap) winSize winSize (Just 24)
  gc <- gcNewWithValues p (newGCValues {foreground = c_black})
  drawRectangle p gc True 0 0 winSize winSize
  return p

initPixmap :: IO Pixmap
initPixmap = do
  pm <- blankPm
  gc <- gcNewWithValues pm (newGCValues {foreground = Color 0 0 65535})
  drawCircle pm gc True center earthSize
  return pm

type Drawer = Pixmap -> PangoContext -> P.Port -> IO ()

clientDrawAdapter :: Pixmap -> [Drawer] -> PangoContext -> Client -> Client
clientDrawAdapter pm cbs pc cli p = do
  drawPortVals pm pc p
  mapM_ (\c -> c pm pc p) cbs
  cli p

radiusDrawer :: Addr -> Color -> Drawer
radiusDrawer addr clr pm _ prt = do
  let r = scale defaultScale (P.readD0 addr prt)
  gc <- gcNewWithValues pm (newGCValues {foreground = clr})
  drawCircle pm gc False center (2*r)

withVisClient ::[Drawer] -> Client -> (Client -> IO Double) -> IO Double
withVisClient cbs cli cb = do
  initGUI
  pm <- initPixmap
  canvas <- drawingAreaNew
  pc <- widgetGetPangoContext canvas
  ad <- adapterSkipper 100 (clientDrawAdapter pm cbs pc)
  s <- cb $ ad cli
  displayPm pm canvas
  return s

displayPm :: Pixmap -> DrawingArea -> IO ()
displayPm pm canvas = do
  window <- windowNew
  onDelete window (\_ -> return False)
  onDestroy window mainQuit
  set window [windowDefaultWidth := winSize+1, windowDefaultHeight := winSize+1,
              containerBorderWidth := 1, containerChild := canvas]
  gc <- gcNewWithValues pm newGCValues
  onExpose canvas (\_ -> do
                     dw <- widgetGetDrawWindow canvas
                     drawDrawable dw gc pm 0 0 0 0 winSize winSize
                     return True)
  widgetShowAll window
  mainGUI







