{-# LANGUAGE TemplateHaskell #-}

module Mantle.RTL where

import Data.Lens.Template
import Data.Vector.Bit

data RTL = RTL {
    _wires     :: [Wire],
    _registers :: [Register],
    _blocks    :: [Block]
}

data Wire = Wire Int (Maybe Expr)

type WireRef = Int

type Register = Int

type RegRef = Int

data Block =
    Latch
      { condition :: Expr,
        updates   :: [Update] }
  | Sync
      { clock     :: Clock,
        updates   :: [Update] }
  | SyncReset
      { clock     :: Clock,
        updates   :: [Update],
        reset     :: Reset,
        resets    :: [Update] }

newtype Clock = Clock WireRef

newtype Reset = Reset WireRef

data Update = Update RegRef Expr

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

$( makeLens ''RTL )
