module Main where 
import Instructions
import OpParser
import Interpreter
import Test.HUnit
import Satellite
import Debug.Trace
import Text.Printf
import Orbits


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
drawScale = (1 / 80000000)


main = do
  (ops, mem) <- readBin file
  initState <- setup ops mem 
  mem <- memToList initState
              --mapM_ (putStrLn . show) $ zip mem [0..] 
              --mapM_ (putStrLn . show) $ zip ops [0..]
  let blank = const [(2, 0.000),(3,0.000)]
  let acc v (p1:p2:_) = [(2, v * cos ang ), (3, v * sin ang)]
          where ang = atan2 (p2!3 -p1 ! 3) (p2!2 - p1!2)

  let goto (v1,v2) (p1:p2:_) =trace ("hello" ++ show change) change
          where change = [(2, (-(v1 - (p1!2 -p2!2)))),(3, (-(v2 - (p1!3 - p2!3))))]

  --ostates <-runRounds ops (const [ (16000, 2001)] :  ( replicate 1000 blank ++ repeat blank)) initState 10000 []
  --gostates <-runRounds ops (const [(16000, 2001)] :  (repeat blank ++replicate 7284 blank ++ [acc 458] ++ replicate 3197 blank ++ [acc 431] ++ repeat blank)) initState 1100 []

  ostates <-runRounds ops (const [(16000, 3004)] :  (replicate 7750 blank ++ [goto  (8354, -457)] ++ repeat blank)) initState 20000 []

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
  let both ((p1x, p1y),(p2x, p2y))= ((p2x, p2y), ((p2x - p1x),(p2y -p1y)))

  let allmypos = map portsToPos ports  
  let allthempos = map portsToThem ports

  --mapM_ (\(step, ((x,y), (x',y'))) -> printf "%d <%f, %f> <%f, %f>\n" (step::Integer) (x::Double) (y::Double) (x'::Double) (y'::Double)) $ tail $ zip [0..1000] $ zip allmypos allthempos

  let points = allmypos
  let them = allthempos
  --
  --let them = zip [0..100000] $ map both $ zip allthempos $ tail allthempos 
  --putStrLn $ show (points,them)
  --  let rad = (head ports) ! 4
  --print $ show rad
  --print $ show $ map portsToPos ports
              


  --print "Starting to draw"
  initScreen
  
  SDL.glSwapBuffers
  --forkIO $ showEach them points
  Draw.draw $ Draw.scale drawScale drawScale $ mconcat $ (map (drawPoint (255,0,0,255)) them ++ drawPoint (0,255,255, 255) (-6539890.002323504,-473379.1685163028) : map (drawPoint (255,255,0,255)) points ++ [drawEarth])
  
  SDL.glSwapBuffers              

  waitClicks
 
  SDL.waitEvent
    
  SDL.quit

  return () 
    where
      showEach [] [] = return ()
      showEach (t:them) (p:points) = do
          Draw.draw $ Draw.scale drawScale drawScale $ mconcat $ [drawPoint (255,0,0,255) t, drawPoint (255,255,0,255) p, drawEarth, drawPoint (0,255,255, 255) (-6539890.002323504,-473379.1685163028)]
          threadDelay 1
          SDL.glSwapBuffers              
          showEach (drop 100 them) (drop 100 points)

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