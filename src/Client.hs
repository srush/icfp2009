module Client where

import Network
import Port (Port)
import qualified Port as P
import IO
import Communication
import Util
import Control.Monad.State

data ClientResult = InputPort Port
                  | Finished

type Client = Port -> IO ClientResult

connect :: String -> Int -> IO Handle
connect s p = connectTo s (PortNumber (toEnum p))

initConn :: String -> Int -> Double -> IO Handle
initConn host p cfg = do
  h <- connect host p
  let prt = P.singleton P.configPort cfg
  writePort h prt
  return h

clientReadLoop :: Handle -> Double -> Client -> IO Double
clientReadLoop h cfg client = do
  oprt <- readPort h
  mIport <- client oprt
  case mIport of
    InputPort iport -> do
                writePort h (P.insert P.configPort cfg iport)
                clientReadLoop h cfg client
    Finished -> return . P.readScore $ oprt

runClient :: String -> Int -> Double -> Client -> IO Double
runClient host port config cli = do
  h <- initConn host port config
  clientReadLoop h config cli

runStateClient :: String -> Int -> Double -> (Port -> State s ClientResult) ->
                  s -> IO Double
runStateClient host port config cli cliinit = do
  cli' <- encapsulateState cli cliinit
  runClient host port config cli'

retPort :: Monad m => Port -> m ClientResult
retPort = return . InputPort
