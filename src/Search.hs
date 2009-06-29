module Main where 


import Orbits
import Battins
import Math
import Data.List
import Text.Printf
import Data.Array
import Data.Function
import Data.Maybe
import Debug.Trace
import qualified Data.Map as M
import Text.ParserCombinators.Parsec hiding (many, optional, (<|>))
import Text.ParserCombinators.Parsec.Language (emptyDef)
import qualified Text.ParserCombinators.Parsec.Token as P
import Control.Applicative
import Numeric
import ClearSkies
import Simulation
import Data.Binary
--import Data.DeriveTH
--import Data.Derive.Eq
--import Data.Derive.Binary


lexer = P.makeTokenParser emptyDef
integer = P.integer lexer


type Pos = (Double, Double)
type Vel = (Double, Double)
type Init = (Pos, Vel)


lambert :: Attempt -> Maybe (Vel, Vel)
lambert  (Attempt {travelTime = time, start = (Sat { pos =(x,y)}), end = (Sat {pos = (x',y')}), shortPath = short}) =
    battins ((x/1000, y/1000), (x'/1000,y'/1000), fromIntegral time, short)
-- trace ("battins: " ++ (show (x,y))++ (show (x', y')) ++ (show time))$


maxFuel = 10000.0

convertToPath :: Attempt -> Maybe (Vel, Vel)
convertToPath a = do
    (vstart, vend) <- lambert a
    return (1000 `pMul` vstart, 1000 `pMul` vend)

verifyLambert p v time (x,y) = --if not ret then trace ("closeness:" ++ show closeness ++ "\n mag: " ++ show mag )  ret else ret 
    ret
    where 
      mag = vecMag (x,y) 
      closeness = vecMag (dx,dy) 
      ret = (closeness / mag) < (1/100)
      dy = abs (y-y')
      dx = abs (x-x')
      ((x',y'), _) =  integratePos p v time

score (a, p) =
    case p of
      Just (vstart, vend) ->  --trace ("wait time" ++ (show $ waitTime a) ++ "travel time" ++ (show $travelTime a)) sc
           --if abs (cos $ angBetweenVects vstart (vel $ start a)) < 0.95 then 1000000000.0
           --else sc
          sc
          where
            --sc = log(fromIntegral (waitTime a) + fromIntegral (travelTime a)) + (45.0/ maxFuel) * (f1 + f2) + (if (f1+f2) > maxFuel then 100000000.0 else 0.0)
            sc = f1 -- + (5000 * eccentricity vend (pos $ end a))  -- no f2 for now + f2   -- + (fromIntegral (waitTime a) + fromIntegral(travelTime a))/2000
            f1 = vecMag $ vstart `pSub` (vel $ start a)
            f2 = vecMag $ (vel $ end a) `pSub` vend
      Nothing -> 1000000000.0

-- trace ("Lambert: " ++(show res) ++ "\n Wait :" ++  (show wait) ++ "\n Travel :" ++  (show travel) ++ "\nScore: " ++ show ret ++ "\n Start: " ++ show start ++ "\n End: " ++show end)
--travelCost :: Bool -> (Int , Int , Init , Init) -> Double
--travelCost short (wait, travel, start, end) =  ret
--   where
--     ret =  case res of
--      Just (vstart, vend) ->
--          log(fromIntegral wait + fromIntegral travel) + (45.0/ maxFuel) * (f1 + f2) + (if (f1+f2) > maxFuel then 100000000.0 else 0.0)
--          where f1 = vecMag $ (1000 `pMul` vstart) `pSub` vcur
--                f2 = vecMag $ vfinish `pSub` (1000 `pMul` vend)
--      Nothing -> 1000000000.0

--     res = lambert short pos endpos travel
--     ( pos, vcur ) = start
--     (endpos, vfinish) = end


showCos (route, p) =
    case p of
      Just (vstart, vend) -> do
          print $ show $ vstart 
          print $ show $ (vel $ start route)
          print $ show $ between 
          where
            between = angBetweenVects (vel $ start route) vstart 

verifyLambert' (route, p) =
    --trace ("verifying " ++ " " ++ show (pos $ start route)  ++ " " ++ show (pos $ end route)  ++ " " ++  show (travelTime route) ++ " " ++ show p ++ " " ++ show (shortPath route))  $ 
          case p of
      Just (vstart, vend) ->
          ((a - (a*e)) > 6357000) &&
          --(vecMag change) > 200 &&
          --abs (cos between) > 0.95  &&
          verifyLambert (pos $ start route) vstart (travelTime route) (pos $ end route)
          --(trace (show (vel $ start route) ++ " " ++ show (abs (cos between)) ++" " ++ show (route, p)) $ verifyLambert (pos $ start route) vstart (travelTime route) (pos $ end route))
          --
          --(abs (angBetweenVects vstart change) < pi/6) &&
    
          where
            between = angBetweenVects (vel $ start route) vstart 
            change =  vstart `pSub` (vel $ start route)
            a = semiMajor (vecMag vstart)  (vecMag $ pos $ start route)
            e = eccentricity vstart $ pos $ start route            
      Nothing -> False 

data Attempt = Attempt {
      waitTime :: Int,
      travelTime :: Int,
      start :: Sat,
      end :: Sat,
      shortPath :: Bool
} deriving (Show)

data Sat = Sat {
      pos :: Pos,
      vel :: Vel
} deriving (Show)




--search ::  Pos -> Vel -> Pos -> Vel -> (Int, Int, Init, Init)
search  getMe getThem timelimit  = -- trace "verified" $ find (verifyLambert'.snd) $ trace ("Finished with: " ++ show (length res'')) res''
    find (verifyLambert'.snd) $ res'' --trace ("Finished with: " ++ show (length res'')) res''
    where
      res''= sortBy (compare `on` fst) (res' res 50000)
      res' r 0 = r 
      res' r n = left --if ((length $ left) < 50) then r
                 --else res' left (n-500)
          where
            left = filter ((n >) . fst) r
      res = map (\s -> (score s, s)) $ map (\a -> (a, convertToPath a))  $ [ Attempt t e (getMe t) (getThem (t+e)) p
                                             | t <-  waitTimes,
                                               e <-  travelTimes,
                                               p <- [False, True],
                                               t + e < timelimit 
                                              ]

      --innerOrb = M.fromList [(7284, ((4837747.318127086,4426116.012086049),(5278.678529087454,-5762.69667944964)))]
      --outerOrb = M.fromList [(7294, (((4837747.318127086,4426116.012086049),(5278.678529087454,-5762.69667944964)))) , (10481, ((-6539890.002323504,-473379.1685163028),(-568.8397054309025,7794.204043423815)))]
      --innerOrb = toOrbitalElements (x,y) (vx, vy)
      --outerOrb = toOrbitalElements (x',y') (vx', vy')
      --atTime map time = toOrbitalState map $ fromIntegral time

parseDouble =
    do s <- getInput
       case readSigned readFloat s of
         [(n, s')] -> n <$ setInput s'
         _         -> empty

readTrace = do
    integer
    char '<'
    x <- parseDouble
    char ','
    char ' '
    y <- parseDouble
    char '>'
    char ' '
    char '<'
    x' <- parseDouble
    char ','
    char ' '
    y' <- parseDouble
    char '>'
    return ((x,y), (x',y'))

readDump = do
    fullTrace <- (sepBy readTrace eol)
    return $ listArray (0, fromIntegral((length fullTrace))) fullTrace

readClearDump = do
    fullTrace <- (sepBy readClearSkies eol)
    return $ listArray (0, fromIntegral((length fullTrace)-1)) fullTrace


readClearSkies = do
  skies <- sepBy parseDouble (char ',')

  return $ toClearSkies $ listArray (0, (length skies - 1))  skies

eol :: Parser Char
eol = char '\n'

fromFile :: String -> IO (Array Int (Pos,Pos))
fromFile filename = do
  contents <- readFile filename
  either (fail.show) (return.id)  $ parse readDump  "" contents

clearFromFile :: String -> IO (Array Int ClearSkies)
clearFromFile filename = do
  contents <- readFile filename
  either (fail.show) (return.id)  $ parse readClearDump  "" contents

searchFrom3Dump dump = do
    posmap <- fromFile dump
    return $ search (innerOrb posmap) (outerOrb posmap)
    where
      innerOrb posmap i = Sat here (here `pSub` old)
          where (here,_) = posmap ! i
                (old,_)  = posmap ! (i-1)

      outerOrb posmap i = Sat here (here `pSub` old)
          where (_, here) = posmap ! i
                (_, old)  = posmap ! (i-1)

searchFrom4dump dump = do
    clearskies <- clearFromFile dump
    let posGetters = sat : [(\sky -> tarpos ((targets sky) !! i)) | i <- [0..10]]
    return $ search (orbber clearskies (posGetters !! 0)) (orbber clearskies (posGetters !! 1))
    where
      orbber clearskies posGetter i = Sat tar ( tar `pSub`  oldtar)
          where tar = posGetter $ (clearskies ! i)
                oldtar = posGetter $ clearskies ! (i-1)


formatOutput n b (a, p) =
    case p of
      Just ((v1, v2), (v1', v2')) ->
          printf "(%d, %s, %d, %f, %f, %d, %f, %f, %f, %f, %s)" (n::Int) (if b then "\"F\"" else "\"T\"")  ((waitTime a)::Int) (v1::Double) (v2::Double) ((travelTime a)::Int) (tx ::Double) (ty::Double) (v1'::Double) (v2'::Double)  (if (shortPath a) then "1" else "0") 
              where (tx,ty) = (pos $ end a) 
      Nothing -> "FAIL"

timeComp = 500
searchFrom4dumpAll dump = do
    clearskies <-   clearFromFile dump
    --return $ map (\ (a,b) -> search (orbber clearskies a) (orbber clearskies b)) $ zip posGetters $ tail posGetters
    --manySearch clearskies posGetters 0 0
    manySearchCont clearskies posGetters (sat (clearskies ! 1), sat (clearskies ! 1) `pSub` sat (clearskies ! 0))   0 0
    where
      orbber skew clearskies posGetter i = Sat tar (tar `pSub`  oldtar)
          where
            arrPos = ((i+skew) `div` timeComp) * 2
            tar = posGetter $ clearskies ! (arrPos +1)
            oldtar = posGetter $ clearskies ! (arrPos)

      customOrber timelimit pos vel i = checkVal i 
          where

            checkVal i = Sat (fst (precomp ! arrPos)) (snd (precomp !arrPos))  
                where arrPos = (i `div` timeComp)
            precomp = posSample pos vel timelimit timeComp  
    
      trash time i  = ((\sky -> tarpos ((targets sky) !! i)), False, time, False)
      -- posGetters = (sat, False, 0) : (map (trash 40000) [0..1] ++ [(refuelPos, True, 50000)] ++ map (trash 70000) [2..3] ++ [(refuelPos, True, 100000)] ++ map (trash 70000) [4] ++ [(refuelPos,True, 70000)] ++ map (trash 100000) [5] ++ [(refuelPos,True, 70000)] ++ map (trash 700000) [6] ++ [(refuelPos,True, 200000)] ++ [(refuelPos,True, 200000)] ++ map (trash 600000) [8] ++ [(refuelPos,True, 200000)] ++ map (trash 600000) [9] )    
      posGetters = (sat, False, 0, False) : (map (trash 70000) [0..1] ++ map (trash 100000) [2..3] ++ map (trash 150000) [4..5] ++ map (trash 300000) [6..7]  ++ map (trash 400000) [8..9])

      manySearch clear [one] _ _ = return []
      manySearch clear ((cur, _, _): (next, b, timelimit): getters) time n = do
         let Just (s,best) =
                 --trace ("Solving with skew " ++ show time ++
                 --        "\n We are " ++ show (orbber time clear cur 1) ++
                 --        "\n They are " ++ show (orbber time clear next 1))
                    search (orbber time clear cur) (orbber time clear next) timelimit
         --let (s, best) = minimumBy (compare `on` fst) $ map (\s -> (score s, s)) res
         putStrLn $ formatOutput n b best
         --print $ show best
         --showCos best
         rest <- manySearch clear ((next,b,timelimit):getters) (time + waitTime (fst best) + travelTime (fst best)) (n + (if b then 0 else 1))
         return $ (s,best) : rest

      manySearchCont clear [one] _ _ _ = return []
      manySearchCont clear ((cur, _, _, _): (next, b, timelimit, sync): getters) (spos, svel) time n = do
         let Just (s,best) = if not sync then 
                                 search (customOrber timelimit spos svel) (orbber time clear next) timelimit
                             else 
                                 search (orbber time clear cur) (orbber time clear next) timelimit
             (a, Just (_,vend)) = best 
             (newpos, newvel) = (pos (end a), vend)
         putStrLn $ formatOutput n b best
         --print $ show best
         --showCos best
         rest <- manySearchCont clear ((next,b,timelimit, sync):getters) (newpos, newvel) (time + waitTime (fst best) + travelTime (fst best)) (n + (if b then 0 else 1))
         return $ (s,best) : rest
 


waitTimes = [2000,2500 .. 400000]

travelTimes = [500,1000..200500]




main = do
  --contents <- fromFile "bin3.obf_3004.dump"
  --print $ show contents
  --let (usmap, themmap ) = (M.fromList us, M.fromList them)
  --print $ usmap ! 7294
  s <- searchFrom4dumpAll "bin4.obf_4001_every500-501.dump"
  print $ show s
  return ()
  --s <- searchFrom4dumpAll "bin4.obf_4001_200K.dump"
  --print $ show $ s
  --mapM_ (\s -> putStrLn$ show s) $ sortBy (compare `on` fst)  $ map (\s -> (score s, s)) s
  --clearskies <- clearFromFile "bin4.obf_4001_every100-101.dump"
  --encodeFile "bin4001.bindump" clearskies
-- $( derive makeBinary ''ClearSkies )
