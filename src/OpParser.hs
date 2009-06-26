{-# OPTIONS_GHC -fglasgow-exts #-}
module OpParser where

import Instructions
import Data.Int
import Data.Bits
import qualified Data.ByteString as BS
import GHC.Exts
import GHC.Prim
import GHC.Word

getBits :: Int -> Int -> Int32 -> Int32
getBits start len op = op' .&. mask
    where
      op' = op `shiftR` start
      mask = (1 `shiftL` len) - 1

parseOp :: Int32 -> OpCode
parseOp i =
    case op of
      0 -> parseSType i
      1 -> Add r1 r2
      2 -> Sub r1 r2
      3 -> Mult r1 r2
      4 -> Div r1 r2
      5 -> Output r1 r2
      6 -> Phi r1 r2
    where
      op = getBits 28 4 i
      r1 = getBits 14 14 i
      r2 = getBits 0 14 i

parseSType :: Int32 -> OpCode
parseSType i =
    case op of
      0 -> Noop
      1 -> Cmpz cmpop r1
      2 -> Sqrt r1
      3 -> Copy r1
      4 -> Input r1
  where
    op = getBits 24 4 i
    cmpop = case getBits 14 4 i of
              0 -> LTZ
              1 -> LEZ
              2 -> EQZ
              3 -> GEZ
              4 -> GTZ
    r1 = getBits 0 13 i

toDouble :: Word64 -> Double
toDouble (W64# b#) = D# (unsafeCoerce# b#)

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


parseDouble :: BS.ByteString -> Double
parseDouble bs = toDouble . compBytes64 . BS.unpack . BS.take 8 $ bs

parseInt32 :: BS.ByteString -> Int32
parseInt32 bs = fromIntegral . compBytes . BS.unpack . BS.take 4 $ bs


_parseBinO :: BS.ByteString -> [(OpCode,Double)]
_parseBinO bs | BS.length bs == 0 = []
             | otherwise = (op, dat) : _parseBinE bs'
    where
      op = parseOp . parseInt32 $ bs
      dat = parseDouble (BS.drop 4 bs)
      bs' = BS.drop 12 bs

_parseBinE :: BS.ByteString -> [(OpCode,Double)]
_parseBinE bs | BS.length bs == 0 = []
             | otherwise = (op, dat) : _parseBinO bs'
    where
      op = parseOp . parseInt32 $ (BS.drop 8 bs)
      dat = parseDouble bs
      bs' = BS.drop 12 bs

parseBin :: BS.ByteString -> ([OpCode],[Double])
parseBin bs = unzip $ _parseBinE bs

readBin :: String -> IO ([OpCode],[Double])
readBin filename = do
  bs <- BS.readFile filename
  return $ parseBin bs

file = "../bins/bin1.obf"
str = BS.pack $ map (fromIntegral.fromEnum) "\t\SOH\NUL\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL"
b = BS.readFile file
bin = readBin file
