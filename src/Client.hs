module Client where

import Network
import Port (Port)
import qualified Port as P
import IO
import System.IO
import Data.IORef
import Communication
import Util
import Control.Monad.State
import System.Environment
import Interpreter
import qualified OpParser as OP

data ClientResult = InputPort Port
                  | Finished

type Client = Port -> IO ClientResult

connect :: String -> Int -> IO Handle
connect s p = connectTo s (PortNumber (toEnum p))

initConn :: String -> Int -> Double -> IO Handle
initConn host p cfg = do
  print $ "Connecting to host " ++ host
  h <- connect host p
  print  "Connected, writing port"
  let prt = P.singleton P.configPort cfg
  writePort h prt
  print "Done"
  return h

clientReadLoop :: Handle -> Double -> Int -> Client -> IO Double
clientReadLoop h cfg limit client = do
  oprt <- readPort h
  mIport <- client oprt
  if limit == 0 then
      return $ P.readScore oprt
   else
       case mIport of
         InputPort iport -> do
                     writePort h (P.insert P.configPort cfg iport)
                     clientReadLoop h cfg (limit-1) client
         Finished -> return $ P.readScore oprt

runClientOnServer :: String -> Int -> Double -> Int -> Client -> IO Double
runClientOnServer host port config limit cli = do
  h <- initConn host port config
  clientReadLoop h config limit cli

clientRunLoop :: (OrbitState -> IO OrbitState) -> Client -> OrbitState -> Int
              -> IO Double
clientRunLoop prog cli st limit = do
  let op = outPort st
  iprtM <- cli op
  if limit == 0 then
      return $ P.readScore op
   else
       case iprtM of
         InputPort iprt -> do
                    st' <- prog (st {inPort = iprt})
                    clientRunLoop prog cli st' (limit-1)
         Finished -> return $ P.readScore op


runClientLocally :: String -> Double -> Int -> Client -> IO Double
runClientLocally file cfg limit cli = do
  (ops,d) <- OP.readBin file
  state <- setup ops d
  let prog = quasiCompile ops
  state' <- prog (state {inPort = P.singleton P.configPort cfg})
  clientRunLoop prog cli state' limit

retPort :: Monad m => Port -> m ClientResult
retPort = return . InputPort

clientWriteAdapter :: Handle -> Client -> Client
clientWriteAdapter h cli prt = do
  hPutStrLn h (P.csvPort prt)
  cli prt

withWriterClient :: String -> Client -> (Client -> IO Double) -> IO Double
withWriterClient file cli cb = do
  withFile file WriteMode $
               \th ->
                 cb (clientWriteAdapter th cli)

adapterSkipper :: Int -> (Client -> Client) -> IO (Client -> Client)
adapterSkipper freq adapter = do
  ios <- newIORef 0
  return $ \cl prt -> do
    c <- readIORef ios
    if c == freq then do
                   writeIORef ios 0
                   adapter cl prt
     else do
       writeIORef ios (c+1)
       cl prt


