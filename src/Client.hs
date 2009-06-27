module Client where

import Network
import Port (Port)
import qualified Port as P
import IO
import System.IO
import Communication
import Util
import Control.Monad.State
import System.Environment

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

clientReadLoop :: Handle -> Double -> Client -> Handle -> IO Double
clientReadLoop h cfg client th = do
  oprt <- readPort h
  mIport <- client oprt
  hPutStrLn th (P.csvPort oprt)
  case mIport of
    InputPort iport -> do
                writePort h (P.insert P.configPort cfg iport)
                clientReadLoop h cfg client th
    Finished -> return $ P.readScore oprt

runClient :: String -> Int -> Double -> Client -> Handle -> IO Double
runClient host port config cli th = do
  h <- initConn host port config
  clientReadLoop h config cli th

retPort :: Monad m => Port -> m ClientResult
retPort = return . InputPort

clientMain :: Client -> IO ()
clientMain cli = do
   [hostS, portS, cfgS, traceS] <- getArgs
   withFile traceS WriteMode $ \th -> do
       score <- runClient hostS (read portS) (read cfgS) cli th
       print score

