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
drawScale = (1 / 20000000)


main = do
  (ops, mem) <- readBin file
  initState <- setup ops mem 
  mem <- memToList initState
              --mapM_ (putStrLn . show) $ zip mem [0..] 
              --mapM_ (putStrLn . show) $ zip ops [0..]
  let blank = const [(2, 0.000),(3,0.000)]
  let acc v (p1:p2:_) = [(2, v * cos ang ), (3, v * sin ang)]
          where ang = atan2 (p2!3 -p1 ! 3) (p2!2 - p1!2)
  ostates <-runRounds ops (const [(2, 8444 -13), (3, -3911+ 7814), (16000, 2001)] :  ( replicate 1000 blank ++ repeat blank)) initState 10000 []
  --ostates <-runRounds ops (const [(16000, 2001)] :  (replicate 7284 blank ++ [acc 458] ++ replicate 3197 blank ++ [acc 431] ++ repeat blank)) initState 50000 []
  --ostates <-  runRounds ops ([(16000, 2001)] :  ([[(2,0), (3, -1502)]] ++ replicate 11978 blank ++ [[(2,0), (3, 1044)]] ++ replicate 3196 blank ++ [[(2,0), (3,328)]] ++ repeat blank)) initState 50000
 
  --ostates <-  runRounds ops ([(16000, 3001)] :  (replicate 2634 blank ++ [[(2,0.0), (3, 1172)]] ++ replicate 4726 blank ++ [[(2, 0.0), (3, -367.63)]] ++ repeat blank)) initState 50000
  -- ostates <-  runRounds ops ([(16000, 3004)] :  (replicate 2573 blank ++ [[(2,-1204), (3, 0.0)]] ++ replicate 4690 blank ++ [[(2, 391), (3, 0.0)]] ++ repeat blank)) initState 50000 
 -- ostates <-  runRounds ops ([(16000, 3003)] :  (replicate 3 blank ++ [[(2,-224), (3, 0.0)]] ++ replicate 3457 blank ++ [[(2, 0), (3, -4310)]] ++ repeat blank)) initState 75000 

  --ostates <-  runRounds ops ([(16000, 3004)] :  (repeat blank ++ [[(2,0.0), (3, 2163)]] ++ replicate 434531 blank ++ [[(2, 0.0), (3, -1010)]] ++ repeat blank)) initState 50000
 
        --ostates <-  runRounds ops ([(16000, 3001)] : (repeat [(2, 0.000),(3,0.000)])) initState 100000

              --mem2 <- memToList ostate
              --print $ show mem2               
              --mapM_ showPorts ostates
  -- mapM_ showPorts ostates
  ports <- mapM getPorts ostates
  let points = map portsToPos ports
  let them = map portsToThem ports
  --  let rad = (head ports) ! 4
  --print $ show rad
  --print $ show $ map portsToPos ports
              


  print "Starting to draw"
  initScreen
  
  SDL.glSwapBuffers
  --forkIO $ showEach them points
  Draw.draw $ Draw.scale drawScale drawScale $ mconcat $ ({-map (drawPoint (255,0,0,255)) them ++ -} map (drawPoint (255,255,0,255)) points ++ [drawEarth, drawPoint (0,255,255, 255) (-6556995, 10000000)])
  
  SDL.glSwapBuffers              

  waitClicks
 
  SDL.waitEvent
    
  SDL.quit

  return () 
    where
      showEach [] [] = return ()
      showEach (t:them) (p:points) = do
          Draw.draw $ Draw.scale drawScale drawScale $ mconcat $ [{-drawPoint (255,0,0,255) t,-} drawPoint (255,255,0,255) p, drawEarth, drawPoint (0,255,255, 255) (-6556995, 10000000)]
          threadDelay 1
          SDL.glSwapBuffers              
          showEach (drop 1 them) (drop 1 points)

      showPorts st = do
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

drawPoint col p = Draw.color col $ Draw.translate p $ Draw.scale 100000 100000 Draw.circle

drawRad r = mappend (Draw.color (0,0,0, 0) $ Draw.scale (r-80000) (r-80000) Draw.circle) (Draw.color (255,0,0, 255) $ Draw.scale r r Draw.circle)