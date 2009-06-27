{-# OPTIONS_GHC -fglasgow-exts #-}
module BitUtils where

import Data.Int
import Data.Bits
import GHC.Exts
import GHC.Prim
import GHC.Word

getBits :: Int -> Int -> Int32 -> Int32
getBits start len op = op' .&. mask
    where
      op' = op `shiftR` start
      mask = (1 `shiftL` len) - 1

composeBytesH :: [Word8] -> Int -> Word32
composeBytesH [] n = 0
composeBytesH (h:t) n = (shiftL (fromIntegral (fromEnum h) :: Word32) (8*n)) .|. (composeBytesH t (n+1))
compBytes :: [Word8] -> Word32
compBytes b = composeBytesH b 0 --((length b)-1)

composeBytes64H :: [Word8] -> Int -> Word64
composeBytes64H [] n = 0
composeBytes64H (h:t) n = (shiftL (fromIntegral (fromEnum h) :: Word64) (8*n)) .|. (composeBytes64H t (n+1))
compBytes64 :: [Word8] -> Word64
compBytes64 b = composeBytes64H b 0 --((length b)-1)


w64ToDouble :: Word64 -> Double
w64ToDouble (W64# b#) = D# (unsafeCoerce# b#)
wordsToDouble :: [Word8] -> Double
wordsToDouble = w64ToDouble . compBytes64

doubleToW64 :: Double -> Word64
doubleToW64 (D# d#) = W64# (unsafeCoerce# d#)

getByte :: Bits a => a -> Int -> a
getByte i b= 255 .&. (shiftR i (8*b))
_getBytes :: Int32 -> Int -> Int -> [Char]
_getBytes i n t | n == t = []
                | otherwise =
                    (toEnum . fromIntegral $ (getByte i n) :: Char) :
                      (_getBytes i (n+1) t)

getBytes :: Int32 -> [Char]
getBytes i = _getBytes i 0 4

_getBytes64 :: Word64 -> Int -> Int -> [Char]
_getBytes64 i n t | n == t = []
                  | otherwise =
     (toEnum (fromIntegral (getByte i n) :: Int) :: Char):
       (_getBytes64 i (n+1) t)

getBytes64 :: Word64 -> [Char]
getBytes64 i = _getBytes64 i 0 8
