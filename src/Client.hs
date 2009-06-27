module Client where

import Network
import Interpreter
import IO
import Communication
import qualified Data.Map as M
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
  let prt = M.singleton configPort cfg
  writePort h prt
  return h

clientReadLoop :: Handle -> Client -> IO Double
clientReadLoop h client = do
  oprt <- readPort h
  mIport <- client oprt
  case mIport of
    InputPort iport -> do
                writePort h iport
                clientReadLoop h client
    Finished -> return . readScore $ oprt

runClient :: String -> Int -> Double -> Client -> IO Double
runClient host port config cli = do
  h <- initConn host port config
  clientReadLoop h cli

runStateClient :: String -> Int -> Double -> (Port -> State s ClientResult) ->
                  s -> IO Double
runStateClient host port config cli cliinit = do
  cli' <- encapsulateState cli cliinit
  runClient host port config cli'
