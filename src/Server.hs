module Server where

import Data.Int
import IO
import Network
import BitUtils
import Interpreter
import qualified Data.Map as M

readChars :: Handle -> Int -> IO String
readChars _ 0 = return []
readChars h n = do
  c <- hGetChar h
  rest <- readChars h (n-1)
  return $ c : rest

readDouble :: Handle -> IO Double
readDouble h = do
  cs <- readChars h 8
  return . wordsToDouble . map (fromIntegral . fromEnum) $ cs

readDoubles :: Handle -> Int32 -> IO [Double]
readDoubles _ 0 = return []
readDoubles h n = do
  d <- readDouble h
  rest <- readDoubles h (n-1)
  return $ d : rest

readInt32 :: Handle -> IO Int32
readInt32 h = do
  cs <- readChars h 4
  return . fromIntegral . compBytes . map (fromIntegral . fromEnum) $ cs

writeDouble :: Handle -> Double -> IO ()
writeDouble h d =  hPutStr h (getBytes64 . doubleToW64 $ d)

writeDoubles :: Handle -> [Double] -> IO ()
writeDoubles h [] = return ()
writeDoubles h (d:t) = do
  writeDouble h d
  writeDoubles h t

writeInt32 :: Handle -> Int32 -> IO ()
writeInt32 h i = do
  hPutStr h (getBytes i)

readPair :: Handle -> IO (Int32, Double)
readPair h = do
  i <- readInt32 h
  d <- readDouble h
  return $ (i, d)

readPairs :: Handle -> Int32 -> IO [(Int32, Double)]
readPairs h 0 = return []
readPairs h n = do
  p <- readPair h
  rest <- readPairs h (n-1)
  return $ p : rest

portReader :: Handle -> IO Port
portReader h = do
  n <- readInt32 h
  ps <- readPairs h n
  return $ M.fromList ps

writePair :: Handle -> (Int32, Double) -> IO ()
writePair h (i,d) = do
  writeInt32 h i
  writeDouble h d

portWriter :: Handle -> Port -> IO ()
portWriter h p = do
  let ps = M.toList p
  mapM_ (writePair h) ps

simServe :: SimBinary -> Handle -> IO ()
simServe b h = do
  stepToCompletion b (portReader h) (portWriter h)

accept_loop :: Socket -> (Handle -> IO()) -> IO ()
accept_loop s serve = do
  (h, _, _) <- accept s
  serve h
  accept_loop s

server :: Int -> SimBinary -> IO ()
server port b = do
  sock <- listenOn (PortNumber (toEnum port))
  accept_loop sock (simServe b)
