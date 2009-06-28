
module Main where 
import Orbits
import Battins
import Math
import Data.List 
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

score (a, p) = 
    case p of 
      Just (vstart, vend) -> 
          log(fromIntegral (waitTime a) + fromIntegral (travelTime a)) + (45.0/ maxFuel) * (f1 + f2) + (if (f1+f2) > maxFuel then 100000000.0 else 0.0)
          where f1 = vecMag $ vstart `pSub` (vel $ start a)
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
      
          
waitTimes = [500,750 .. 40000]
travelTimes = [100,150..9500]


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
search  (posmap) = 
    filter ((30>) . score) $  map (\a -> (a, convertToPath a))  $ [ Attempt t e (atTime innerOrb t) (atTime outerOrb (t+e)) p
                                             | t <-  waitTimes,
                                               e <-  travelTimes,
                                               p <- [False, True]
                                              ]
                                    
    where
      --innerOrb = M.fromList [(7284, ((4837747.318127086,4426116.012086049),(5278.678529087454,-5762.69667944964)))]
      --outerOrb = M.fromList [(7294, (((4837747.318127086,4426116.012086049),(5278.678529087454,-5762.69667944964)))) , (10481, ((-6539890.002323504,-473379.1685163028),(-568.8397054309025,7794.204043423815)))]
      --innerOrb = toOrbitalElements (x,y) (vx, vy)
      --outerOrb = toOrbitalElements (x',y') (vx', vy')
      --atTime map time = toOrbitalState map $ fromIntegral time
      innerOrb i = Sat here (here `pSub` old)
          where (here,_) = posmap ! i
                (old,_)  = posmap ! (i-1)  
                    
      outerOrb i = Sat here (here `pSub` old)
          where (_, here) = posmap ! i
                (_, old)  = posmap ! (i-1)  
      atTime map time = map time

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

eol :: Parser Char
eol = char '\n'

fromFile :: String -> IO (Array Int (Pos,Pos))
fromFile filename = do
  contents <- readFile filename
  either (fail.show) (return.id)  $ parse readDump  "" contents

main = do
  contents <- fromFile "bin3.obf_3004.dump"
  --print $ show contents
  --let (usmap, themmap ) = (M.fromList us, M.fromList them)
  --print $ usmap ! 7294 
  let s = search contents  
  mapM_ (\s -> putStrLn$ show s) $ sortBy (compare `on` fst)  $ map (\s -> (score s, s)) s