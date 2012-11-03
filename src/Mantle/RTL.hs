{-# LANGUAGE Rank2Types #-}

module Mantle.RTL where

import Data.Vector.Bit

data RTL = RTL {
    logic      :: [Maybe (Int,Expr)],
    registers  :: [Register],
    triggers   :: [Trigger]
}

data Expr = Lit BitVector
          | Var LogicRef
          | Acc RegRef
          | BinOp Expr BinaryOperator Expr
          | UnOp UnaryOperator Expr
          | Cond Expr Expr Expr
          | BitSel Expr Expr -- Left must be name, needs better solution
          | BitRange Expr Int Int
          | Concat [Expr]

type LogicRef = Int

type RegRef = Int

data BinaryOperator = OpAdd | OpSub | OpMul | OpDiv | OpMod
                    | OpAnd | OpOr
                    | OpEqual | OpNotEq
                    | OpLT | OpGT | OpLTE | OpGTE
                    | OpShiftL | OpShiftR
                    | OpBitAnd | OpBitOr | OpBitXor
                    -- ...

data UnaryOperator = OpNegate | OpNot -- ...

data Register = Register Trigger LogicRef

data Trigger = PosEdge LogicRef
             | NegEdge LogicRef
             | BothEdge LogicRef
             | Trigs [Trigger]
