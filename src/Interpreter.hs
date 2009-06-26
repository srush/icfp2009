module Interpreter where 
import Data.Array.ST


type Memory = STUArray s Int Int
type Port = STUArray s Int Int

data OrbitState = OrbitState {
      status :: Bool,
      port  :: Port,
      mem :: Memory
} 




step :: OrbitState -> (Instruction, Step) -> ST OrbitState 
step state (ins, rd) = 
    case ins of 
      (Add r1 r2) -> do
               v1 <- get r1
               v2 <- get r2
               write v1 + v2 
      (Sub r1 r2) -> do
               v1 <- get r1
               v2 <- get r2
               write v1 - v2 
      (Mult r1 r2) -> do
               v1 <- get r1
               v2 <- get r2
               write v1 * v2
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
      (Noop) -> 
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
      where write r v = do
               writeArray (mem state) r v
               return state
            getPort = readArray (port state)
            writePort p v = do 
               writeArray (port state) v
               return state
            setStatus b = return $ state {status = b} 
            get = readArray (mem state)


-- Warning: memory size and instruction size must match 
setup :: [Instruction] -> [Double] -> ST OrbitState
setup ins mem = do 
  initMem <- newListArray (1, length ins) mem 
  initPort <- newArray (1, length ins) 0.0 :: Port
  let  initState = OrbitState {
                     status = 0,
                     mem = mem,
                     port = initPort}
  

runRound :: [Instruction] -> OrbitState -> ST OrbitState 
runRound ins start = doOne ordered start 
  where ordered = zip ins [1..]
        doOne [] orbit = return orbit 
        doOne (cur:ins) orbit = do 
             nextOrbit <- step cur orbit
             doOne ins nextOrbit
            

