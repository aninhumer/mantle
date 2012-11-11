
module Mantle.RTL where

import Data.Vector.Bit

data RTL = RTL {
    wires     :: [Wire],
    registers :: [Register],
    blocks    :: [Block]
}

data Wire = Wire Int (Maybe Expr)

type WireRef = Int

type Register = Int

type RegRef = Int

data Block =
    Latch
      { condition :: WireRef,
        updates   :: [Update] }
  | Sync
      { clock     :: WireRef,
        updates   :: [Update] }
  | SyncReset
      { clock     :: WireRef,
        updates   :: [Update],
        reset     :: WireRef,
        resets    :: [Update] }

data Update = Update RegRef WireRef

data Expr = Lit BitVector
          | Var WireRef
          | Acc RegRef
          | BinOp Expr BinaryOperator Expr
          | UnOp UnaryOperator Expr
          | Cond Expr Expr Expr
          | BitSel Expr Expr -- Left must be name, needs better solution
          | BitRange Expr Int Int
          | Concat [Expr]


data BinaryOperator = OpAdd | OpSub | OpMul | OpDiv | OpMod
                    | OpAnd | OpOr
                    | OpEqual | OpNotEq
                    | OpLT | OpGT | OpLTE | OpGTE
                    | OpShiftL | OpShiftR
                    | OpBitAnd | OpBitOr | OpBitXor
                    -- ...

data UnaryOperator = OpNegate | OpNot -- ...

