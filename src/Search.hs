
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
import Data.DeriveTH
import Data.Derive.Eq
import Data.Derive.Binary


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

verifyLambert p v time (x,y) = (abs (x - x') < 100) && (abs (y - y') < 100)    
    where ((x',y'), _) = integratePos p v time

score (a, p) = 
    case p of 
      Just (vstart, vend) ->   trace ("wait time" ++ (show $ waitTime a) ++ "travel time" ++ (show $travelTime a)) sc 
           
          where 
            --sc = log(fromIntegral (waitTime a) + fromIntegral (travelTime a)) + (45.0/ maxFuel) * (f1 + f2) + (if (f1+f2) > maxFuel then 100000000.0 else 0.0)
            sc = f1 + f2
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
      
          
verifyLambert' (a, p) =
    case p of 
      Just (vstart, vend) ->  
          verifyLambert (pos $ start a) vstart (travelTime a) (pos $ end a)     
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
search  getMe getThem = find (verifyLambert'.snd) res'                                     
    where
      res'= trace ("Finished with: " ++ show (length res)) $ sortBy (compare `on` fst) $ map (\s -> (score s, s)) res 
      res = filter ((5000 >) . score) $  map (\a -> (a, convertToPath a))  $ [ Attempt t e (getMe t) (getThem (t+e)) p
                                             | t <-  waitTimes,
                                               e <-  travelTimes,
                                               p <- [False, True]
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


formatOutput n (a, p) = 
    case p of 
      Just ((v1, v2), _) ->
          printf "lambert(%d, %d, %f, %f, %d);" (n::Int) ((waitTime a)::Int) (v1::Double) (v2::Double) ((travelTime a)::Int)
      Nothing -> "FAIL"

timeComp = 500
searchFrom4dumpAll dump = do
    clearskies <-   clearFromFile dump 
    --return $ map (\ (a,b) -> search (orbber clearskies a) (orbber clearskies b)) $ zip posGetters $ tail posGetters 
    manySearch clearskies posGetters 0 0
    where 
      orbber skew clearskies posGetter i = Sat tar (tar `pSub`  oldtar) 
          where 
            arrPos = ((i+skew) `div` timeComp) * 2
            tar = posGetter $ clearskies ! (arrPos +1)
            oldtar = posGetter $ clearskies ! (arrPos)

      trash i sky = tarpos ((targets sky) !! i)
      posGetters = sat : (map trash [0..3] ++ [refuelPos] ++ map trash [4..6])    

      manySearch clear [one] _ _ = return []
      manySearch clear (cur: next: getters) time n = do
         let Just (s,best) = 
                 --trace ("Solving with skew " ++ show time ++
                 --        "\n We are " ++ show (orbber time clear cur 1) ++
                 --        "\n They are " ++ show (orbber time clear next 1)) 
                    search (orbber time clear cur) (orbber time clear next)
         --let (s, best) = minimumBy (compare `on` fst) $ map (\s -> (score s, s)) res
         putStrLn $ formatOutput n best
         rest <- manySearch clear (next:getters) (time + waitTime (fst best) + travelTime (fst best)) (n +1)
         return $ (s,best) : rest
 
waitTimes = [500,1000 .. 100000]
travelTimes = [500,1500..70500]




main = do
  --contents <- fromFile "bin3.obf_3004.dump"
  --print $ show contents
  --let (usmap, themmap ) = (M.fromList us, M.fromList them)
  --print $ usmap ! 7294 
  s <- searchFrom4dumpAll "bin4.obf_4001_every500-501.dump"
  return ()
  --s <- searchFrom4dumpAll "bin4.obf_4001_200K.dump"
  --print $ show $ s
  --mapM_ (\s -> putStrLn$ show s) $ sortBy (compare `on` fst)  $ map (\s -> (score s, s)) s
  --clearskies <- clearFromFile "bin4.obf_4001_50k.dump"  
  --encodeFile "bin4001.bindump" clearskies
-- $( derive makeBinary ''ClearSkies )