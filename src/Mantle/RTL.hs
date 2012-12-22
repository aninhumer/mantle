{-# LANGUAGE TemplateHaskell #-}

module Mantle.RTL where

import Control.Lens.TH
import Data.Vector.Bit
import qualified Data.Sequence as S
import qualified Data.Map as M
import qualified Data.Set as Set


data RTL = RTL {
    _vars   :: M.Map Ref Variable,
    _blocks :: M.Map Trigger Block
}

newtype Ref = Ref Int
    deriving (Eq, Ord)

-- Temporary replacement for names
instance Show Ref where
    show (Ref i) = "a" ++ show i

data Variable = Variable {
    _varType :: VarType,
    _width   :: Int
}

data VarType = WireVar | RegVar

type Trigger = Set.Set Edge

data Edge = PosEdge Ref
          | NegEdge Ref
          | EitherEdge Ref
          deriving (Eq,Ord)

type Block = S.Seq Statement

data Statement = Cond Expr Block Block
            | BlockingAssign Ref Expr
            | AsyncAssign Ref Expr

data Expr = Lit BitVector
          | Var Ref
          | BinOp Expr BinaryOperator Expr
          | UnOp UnaryOperator Expr
          | CondE Expr Expr Expr
          | BitSel Ref Expr
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
$( makeLenses ''Variable )
