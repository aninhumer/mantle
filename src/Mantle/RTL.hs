{-# LANGUAGE TemplateHaskell #-}

module Mantle.RTL where

import Control.Lens.TH
import Data.Vector.Bit
import Data.Sequence
import Data.Map

data RTL = RTL {
    _wires     :: Seq Wire,
    _registers :: Seq RegSize,
    _latches   :: Seq Latch,
    _syncs     :: Seq Sync
}

data Wire = Wire {
    _width :: Int,
    _comb  :: Maybe Expr
}

type WireRef = Int

type RegSize = Int

type RegRef = Int

data Latch = Latch {
    _condition    :: Expr,
    _latchUpdates :: RegWrites
}

type LatchRef = Int

data Sync = Sync {
    _clock        :: Clock,
    _clockUpdates :: RegWrites,
    _reset        :: Reset,
    _resetUpdates :: RegWrites
}

type SyncRef = Int

newtype Clock = Clock WireRef

newtype Reset = Reset WireRef

type RegWrites = Map RegRef Expr

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

$( makeLenses ''RTL )
$( makeLenses ''Wire )
$( makeLenses ''Latch )
$( makeLenses ''Sync )
