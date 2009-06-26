module Instructions where

import Data.Int
type Addr = Int32

data CompOp = LTZ
            | LEZ
            | EQZ
            | GEZ
            | GTZ
              deriving Show

data OpCode = Add Addr Addr
            | Sub Addr Addr
            | Mult Addr Addr
            | Div Addr Addr
            | Output Addr Addr
            | Phi Addr Addr
            | Noop
            | Cmpz CompOp Addr
            | Sqrt Addr
            | Copy Addr
            | Input Addr
              deriving Show
