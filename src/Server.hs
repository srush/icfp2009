module Server where

import Interpreter
import Network
import Instructions
import IO
import Communication
import Util
import System.Environment
import OpParser

simServe :: SimBinary -> Handle -> IO ()
simServe b h = do
  stepForever b (readPort h) (writePort h)

accept_loop :: Socket -> (Handle -> IO()) -> IO ()
accept_loop s serve = do
  (h, _, _) <- accept s
  ignoreExns $ serve h
  accept_loop s serve

server :: Int -> SimBinary -> IO ()
server port b = do
  sock <- listenOn (PortNumber (toEnum port))
  accept_loop sock (simServe b)

main :: IO ()
main = do
  [portS, binFile] <- getArgs
  bin <- readBin binFile
  let port = read portS
  server port bin
