module Main where 
import Instructions
import OpParser
import Interpreter
import Test.HUnit
import Satellite

import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.SDL as SDL
--import Data.Array
import Data.Monoid
import Control.Concurrent
import Data.Map ((!))


resX = 1000
resY = 1000

initScreen :: IO ()
initScreen = do
    SDL.init [SDL.InitTimer, SDL.InitVideo]
    -- resolution & color depth
    SDL.setVideoMode resX resY 32 [SDL.OpenGL]
    return ()
drawScale = (1 / 40000000)


main = do
  (ops, mem) <- readBin file
  initState <- setup ops mem 
  mem <- memToList initState
              --mapM_ (putStrLn . show) $ zip mem [0..] 
              --mapM_ (putStrLn . show) $ zip ops [0..]
 
  ostates <-  runRounds ops ([(16000, 3004)] : (repeat [(2, 0.000),(3,0.000)])) initState 2

              --mem2 <- memToList ostate
              --print $ show mem2               
              --mapM_ showPorts ostates
  mapM_ showPorts ostates
  ports <- mapM getPorts ostates
  let points = map portsToPos ports
  let rad = (head ports) ! 4
  --print $ show rad
  --print $ show $ map portsToPos ports
              


  print "Starting to draw"
  initScreen

  SDL.glSwapBuffers
   
  Draw.draw $ Draw.scale drawScale drawScale $ mconcat $ (map drawPoint points ++  [drawEarth, drawRad rad])
      
  SDL.glSwapBuffers
  waitClicks
 
  SDL.waitEvent
    
  SDL.quit

  return () 
    where showPorts st = do
                          ls <- portToList st
                          putStrLn $ show ls
          getPorts st = do 
            portToMap st 
          waitClicks = do
                          ev <- SDL.waitEvent
                          case ev of
                            SDL.Quit -> return ()
                            _ -> waitClicks

drawEarth = Draw.color (0,0,255, 255) $ Draw.scale 6300000 6300000 Draw.circle

drawPoint p = Draw.translate p $ Draw.scale 50000 50000 Draw.circle

drawRad r = mappend (Draw.color (0,0,0, 0) $ Draw.scale (r-80000) (r-80000) Draw.circle) (Draw.color (255,0,0, 255) $ Draw.scale r r Draw.circle)