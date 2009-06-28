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
defaultScale = 50000

scale :: Double -> Double -> Int
scale s v = round (v / s)

-- Scale factor is how many m per pixel
coordToPixel :: Double -> (Double, Double) -> (Int, Int)
coordToPixel s pos = ((round x) + earthPos, (round (-y)) + earthPos)
    where
      (x,y) = pos `pDiv` s

drawCircle :: Pixmap -> GC -> Bool -> (Int, Int) -> Int -> IO ()
drawCircle pm gc fill (x,y) width = drawArc pm gc fill x' y' width width 0 23040
    where
      (x',y') = (x - width `div` 2, y - width `div` 2)

drawSat :: Pixmap -> (Double, Double) -> IO ()
drawSat pm s = do
  let c = coordToPixel defaultScale s
  gc <- gcNewWithValues pm (newGCValues {foreground = Color 65535 0 0})
  drawCircle pm gc True c 5

drawPort :: Pixmap -> P.Port -> IO ()
drawPort pm prt = do
  let s = (P.readSX prt, P.readSY prt)
  drawSat pm s

initPixmap :: IO Pixmap
initPixmap = do
  pm <- pixmapNew (Nothing :: Maybe Pixmap) winSize winSize (Just 24)
  gc <- gcNewWithValues pm (newGCValues {foreground = Color 0 0 65535})
  drawCircle pm gc True center earthSize
  return pm

type Drawer = Pixmap -> P.Port -> IO ()

clientDrawAdapter :: Pixmap -> [Drawer] -> Client -> Client
clientDrawAdapter pm cbs cli p = do
  drawPort pm p
  mapM_ (\c -> c pm p) cbs
  cli p

radiusDrawer :: Addr -> Color -> Pixmap -> P.Port -> IO ()
radiusDrawer addr clr pm prt = do
  let r = scale defaultScale (P.readD0 addr prt)
  gc <- gcNewWithValues pm (newGCValues {foreground = clr})
  drawCircle pm gc False center r

withVisClient ::[Drawer] -> Client -> (Client -> IO Double) -> IO Double
withVisClient cbs cli cb = do
  initGUI
  pm <- initPixmap
  s <- cb $ clientDrawAdapter pm cbs cli
  displayPm pm
  return s

displayPm :: Pixmap -> IO ()
displayPm pm = do
  window <- windowNew
  onDelete window (\_ -> return False)
  onDestroy window mainQuit
  canvas <- drawingAreaNew
  set window [windowDefaultWidth := winSize+1, windowDefaultHeight := winSize+1,
              containerBorderWidth := 1, containerChild := canvas]
  gc <- gcNewWithValues pm newGCValues
  onExpose canvas (\_ -> do
                     dw <- widgetGetDrawWindow canvas
                     drawDrawable dw gc pm 0 0 0 0 winSize winSize
                     return True)
  widgetShowAll window
  mainGUI

main = do
  initGUI
  pm <- initPixmap
  displayPm pm



