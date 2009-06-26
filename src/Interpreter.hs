{-# LANGUAGE RankNTypes #-}


module Interpreter where 
import Data.Array.IO
import Instructions
import Control.Monad.ST
import Test.HUnit

type Memory = IOUArray Addr Double
type Port = IOUArray Addr Double 
type Step = Int 

data OrbitState = OrbitState {
      status :: Bool,
      port  :: Port,
      mem :: Memory
} 




step :: OrbitState -> (OpCode, Addr) -> IO OrbitState 
step state (ins, rd) = 
    case ins of 
      (Add r1 r2) -> do
               v1 <- get r1
               v2 <- get r2
               write $ v1 + v2 
      (Sub r1 r2) -> do
               v1 <- get r1
               v2 <- get r2
               write $ v1 - v2 
      (Mult r1 r2) -> do
               v1 <- get r1
               v2 <- get r2
               write $ v1 * v2
      (Div r1 r2) -> do
               v1 <- get r1
               v2 <- get r2
               if v1 == 0.0 then 
                   write 0.0
                else 
                   write $ v1 / v2
      (Output r1 r2) -> do
               v2 <- get r2
               writePort r1 v2
      (Phi r1 r2) -> do
               v1 <- get r1
               v2 <- get r2
               if status state then write v1
                else write v2
      (Noop) -> return state
      (Cmpz imm r1) -> do 
               v1 <- get r1
               setStatus (v1 `op` 0.0)
          where
            op = case imm of 
                   LTZ -> (<)
                   LEZ -> (<=)
                   EQZ -> (==)
                   GEZ -> (>=)
                   GTZ -> (>)    
      (Sqrt r1) -> do 
               v1 <- get r1
               write $ abs $ sqrt v1
      (Copy r1) -> do 
               v1 <- get r1
               write v1
      (Input r1) -> do 
               p1 <- getPort r1
               write p1
      where write v = do
               writeArray (mem state) rd v
               return state
            getPort = readArray (port state)
            writePort p v = do 
               writeArray (port state) p v
               return state
            setStatus b = return $ state {status = b} 
            get = readArray (mem state)


-- Warning: memory size and instruction size must match 
setup :: [OpCode] -> [Double] -> [Double]-> IO OrbitState
setup ins mem ports = do 
  initMem <- newListArray (1, fromIntegral $ length ins) mem 
  initPort <- newListArray (1, fromIntegral $ length ins) ports
  return OrbitState {
                  status = False,
                  mem = initMem,
                  port = initPort}
 

runRound :: [OpCode] -> OrbitState -> IO OrbitState 
runRound ins start = doOne ordered start 
  where ordered = zip ins [1..]
        doOne [] orbit = return orbit 
        doOne (cur:ins) orbit = do 
             nextOrbit <- step orbit cur 
             doOne ins nextOrbit 

boringState ins = repeat 0

getMem st add =  readArray (mem st) add
memToList st = getElems $ mem st 
portToList st = getElems $ port st 

testData = [([Add 1 2, Noop], [1.0, 2.0], [3.0, 2.0]),
            ([Sub 1 2, Noop], [10.0, 2.0], [8.0, 2.0]),
            ([Mult 1 2, Cmpz EQZ 1, Phi 3 4, Noop],  [1.0, 2.0, 3.0, 5.0], [2.0, 2.0, 5.0, 5.0]),
            ([Mult 1 2, Cmpz EQZ 1, Phi 3 4, Noop],  [1.0, 0.0, 3.0, 5.0], [0.0, 0.0, 3.0, 5.0]),
            ([Copy 1, Copy 1, Copy 1, Copy 1],  [1.0, 0.0, 3.0, 5.0], [1.0, 1.0, 1.0, 1.0])
           ]

testDataPorts = [([Add 1 2, Noop], ([1.0, 2.0], [0.0, 0.0]), ([3.0, 2.0], [0.0, 0.0])),
                 ([Add 1 2, Output 1 1], ([1.0, 2.0], [0.0, 0.0]), ([3.0, 2.0], [3.0, 0.0])),
                 ([Input 1, Add 1 2, Output 2 2], ([1.0, 2.0, 1.0], [17.0, 0.0, 0.0]), ([17.0, 19.0, 1.0], [17.0, 19.0, 0.0]))
                ]


testAdd = TestCase $ mapM_ testOne testData 
    where
      testOne (ins, start, end) = do
          initState <- setup ins start $ repeat 0
          ostate <- runRound ins initState 
          ls <- memToList ostate
          assertEqual "mem check" ls end

testPorts = TestCase $ mapM_ testOne testDataPorts
    where
      testOne (ins, (start, sports), (end, eports)) = do
          initState <- setup ins start sports
          ostate <- runRound ins initState 
          ls <- memToList ostate
          assertEqual "mem check" ls end
          ls' <- portToList ostate
          assertEqual "port check" ls' eports




runTests = TestList $ [testAdd, testPorts]