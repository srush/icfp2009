module Server where

import Network
import Interpreter
import Instructions
import IO
import Communication

simServe :: SimBinary -> Handle -> IO ()
simServe b h = do
  stepToCompletion b (readPort h) (writePort h)

accept_loop :: Socket -> (Handle -> IO()) -> IO ()
accept_loop s serve = do
  (h, _, _) <- accept s
  serve h
  accept_loop s serve

server :: Int -> SimBinary -> IO ()
server port b = do
  sock <- listenOn (PortNumber (toEnum port))
  accept_loop sock (simServe b)
