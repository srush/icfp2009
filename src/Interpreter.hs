{-# LANGUAGE RankNTypes #-}


module Interpreter where
import Data.Array.IO
import Instructions
import Control.Monad.ST
import Control.Exception
import Test.HUnit hiding (assert)
import qualified Data.Map as M


type Memory = IOUArray Addr Double
type Port = M.Map Addr Double
type Step = Int

data OrbitState = OrbitState {
      status :: Bool,
      inPort  :: Port,
      outPort  :: Port,
      mem :: Memory
}


scorePort :: Addr
scorePort = 0

stepToCompletion :: SimBinary -> IO Port -> (Port -> IO ()) -> IO ()
stepToCompletion (op, d) reader writer = return ()

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
               if v2 == 0.0 then
                   write 0.0
                else
                   write (v1 / v2)
      (Output r1 r2) -> do
               v2 <- get r2
               writePort r1 v2
      (Phi r1 r2) -> do
               --print "doing phi"
               --print $ show $ status state
               v1 <- get r1
               v2 <- get r2
               if status state then write v1
                else write v2
      (Noop) -> return state
      (Cmpz imm r1) -> do
               v1 <- get r1
               --print "comparing"
               --print $ show rd
               --print $ show imm
               --print v1
               let b = (v1 `op` 0.0)
               --print $ show b
               setStatus b
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
               --print $ "Writing value "
               --print $ show v
               --print $ show rd
               writeArray (mem state) rd v
               return state
            getPort p = do
                --print $ "Getting value "
                --print $ show p
                --print $ show $ inPort state
                --print $ show $ M.findWithDefault 0.0 p $ inPort state
                return $  M.findWithDefault 0.0 p $ inPort state
            writePort p v = do
               --print $ "Writing Port"
               --print $ (show p) ++ (show v)
               return $ state {outPort = M.insert p v $ outPort state}
            setStatus b = return $ state {status = b}
            get = readArray (mem state)


-- Warning: memory size and instruction size must match
setup :: [OpCode] -> [Double] -> IO OrbitState
setup ins mem = do
  initMem <- newListArray (0, fromIntegral $ (length ins -1)) $ assert (length ins == length mem) $  mem
  return OrbitState {
                  status = False,
                  mem = initMem,
                  inPort = M.empty,
                  outPort = M.empty
             }


runRound :: [OpCode] -> [(Addr, Double)] ->  OrbitState -> IO OrbitState
runRound ins ports st = doOne ordered start
  where ordered = zip ins [0..]
        start = st {inPort = M.fromList ports}
        doOne [] orbit = return orbit
        doOne (cur:ins) orbit = do
             nextOrbit <- step orbit cur
             doOne ins nextOrbit

runRounds :: [OpCode] -> [[(Addr, Double)]] -> OrbitState -> Int -> IO [OrbitState]
runRounds  _ _ _ 0 = return []
runRounds ins (p:ports) start n = do
  next <- runRound ins p start
  more <- runRounds ins ports next (n-1)
  return $ next:more



boringState ins = repeat 0

getMem st add =  readArray (mem st) add
memToList st = getElems $ mem st
portToList st = return $ M.toList $ outPort st
portToMap st = return $ outPort st

testData = [([Add 0 1, Noop], [1.0, 2.0], [3.0, 2.0]),
            ([Sub 0 1, Noop], [10.0, 2.0], [8.0, 2.0]),
            ([Mult 0 1, Cmpz EQZ 0, Phi 2 3, Noop],  [1.0, 2.0, 3.0, 5.0], [2.0, 2.0, 5.0, 5.0]),
            ([Mult 0 1, Cmpz EQZ 0, Phi 2 3, Noop],  [1.0, 0.0, 3.0, 5.0], [0.0, 0.0, 3.0, 5.0]),
            ([Mult 0 1, Cmpz LTZ 0, Phi 2 3, Noop],  [-1.0, 2.0, 3.0, 5.0], [-2.0, 2.0, 3.0, 5.0]),
            ([Copy 0, Copy 0, Copy 0, Copy 0],  [1.0, 0.0, 3.0, 5.0], [1.0, 1.0, 1.0, 1.0]),
            ([Mult 0 0, Mult 1 1, Add 0 1, Sqrt 2 ], [3.0, 4.0, 0.0, 0.0], [9.0, 16.0, 25.0, 5.0]),
            ([Sqrt 0, Mult 0 0, Sub 1 2, Mult 2 2, Sub 4 3, Cmpz GTZ 4, Phi 0 1], [2.0, 0.0, 2.0, 0.0, 0.00001, 0.0, 0.0], [sqrt 2.0, 2.0, 0.0, 0.0, 0.00001, 0.0, sqrt 2.0]),
            ([Div 1 0, Div 1 0, Mult 1 0], [4.0, 24.0, 0.0], [6.0, 4.0, 24.0]),
            ([Sub 0 1, Div 1 0, Mult 2 0], [3.0, 3.0, 5.0], [0.0, 0.0, 0.0]) -- check div by 0
           ]

testDataPorts = [([Add 0 1, Noop], ([1.0, 2.0], [(0,0.0), (1,0.0)]), ([3.0, 2.0], [])),
                 ([Add 0 1, Output 0 0], ([1.0, 2.0], []), ([3.0, 2.0], [(0,3.0)])),
                 ([Input 0, Add 0 1, Output 1 1], ([1.0, 2.0, 1.0], [(0,17.0), (1,0.0), (2,0.0)]), ([17.0, 19.0, 1.0], [(1,19.0)]))
                ]





testAdd = TestCase $ mapM_ testOne testData
    where
      testOne (ins, start, end) = do
          initState <- setup ins start
          ostate <- runRound ins [] initState
          ls <- memToList ostate
          assertEqual "mem check" end ls

testPorts = TestCase $ mapM_ testOne testDataPorts
    where
      testOne (ins, (start, sports), (end, eports)) = do
          initState <- setup ins start
          ostate <- runRound ins sports initState
          ls <- memToList ostate
          assertEqual "mem check" end ls
          ls' <- portToList ostate
          assertEqual "port check" eports $ (take (length eports) ls')






runTests = TestList $ [testAdd, testPorts]
