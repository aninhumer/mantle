{-# LANGUAGE TemplateHaskell #-}

module Mantle.RTL where

import Control.Lens.TH
import Data.Monoid
import qualified Data.Map as M
import qualified Data.Set as Set


data RTL = RTL {
    _inputs  :: M.Map Ref VType,
    _outputs :: M.Map Ref VType,
    _wires   :: M.Map Ref VType,
    _regs    :: M.Map Ref VType,
    _combs   :: M.Map Ref Expr,
    _blocks  :: M.Map Trigger Block
}

newtype Ref = Ref Int
    deriving (Eq, Ord)

data VType = BitType Int
           | VecType Int VType

type Trigger = Set.Set Edge

data Edge = PosEdge Ref
          | NegEdge Ref
          | EitherEdge Ref
          deriving (Eq,Ord)

data Block = Block {
    _conds  :: M.Map Expr Block,
    _writes :: Update
}

type Update = M.Map RegRef Expr

data RegRef = IndexedRef Ref Expr
            | NormalRef Ref
            deriving (Eq,Ord)

data Expr = Lit Value
          | Var Ref
          | BinOp Expr BinaryOperator Expr
          | UnOp UnaryOperator Expr
          | CondE Expr Expr Expr
          | Index Ref Expr
          | BitRange Expr Int Int
          | Concat [Expr]
          deriving (Eq,Ord)

data Value = Dec Integer
           | Undef
           deriving (Eq,Ord)

data BinaryOperator = OpAdd | OpSub | OpMul | OpDiv | OpMod
                    | OpAnd | OpOr
                    | OpEqual | OpNotEq
                    | OpLT | OpGT | OpLTE | OpGTE
                    | OpShiftL | OpShiftR
                    | OpBitAnd | OpBitOr | OpBitXor
                    -- ...
                    deriving (Eq,Ord)

data UnaryOperator = OpNegate | OpNot -- ...
    deriving (Eq,Ord)


instance Monoid RTL where
    mempty = RTL mempty mempty mempty mempty mempty mempty
    mappend (RTL xi xo xw xr xc xb) (RTL yi yo yw yr yc yb) =
        RTL (xi <> yi) (xo <> yo) (xw <> yw) (xr <> yr)
            (xc <> yc) (M.unionWith (<>) xb yb)

instance Monoid Block where
    mempty = Block M.empty M.empty
    mappend (Block xc xu) (Block yc yu) =
        Block (M.unionWith (<>) xc yc) (xu <> yu)


$( makeLenses ''RTL )
$( makeLenses ''Block )
