{-# LANGUAGE TemplateHaskell #-}

module Mantle.RTL where

import Control.Lens.TH
import Data.Monoid
import qualified Data.Map as M
import qualified Data.Set as Set


data RTL = RTL {
    _decls  :: [Declaration],
    _combs  :: M.Map Ref Expr,
    _blocks :: M.Map Trigger Block
}

data Ref = Ref Int
         | Named String
    deriving (Eq, Ord)

-- TODO: BitType with no Int.
data VType = BitType
           | VecType Int VType

data Declaration = Declaration {
    dtype :: DType,
    vtype :: VType,
    dref  :: Ref
}

data DType = DInput | DOutput
           | DWire  | DReg
           deriving (Eq)

type Trigger = Set.Set Edge

data Edge = PosEdge Ref
          | NegEdge Ref
          | EitherEdge Ref
          deriving (Eq,Ord)

data Block = Block {
    _conds  :: M.Map Expr (Block,Block),
    _writes :: Update
} deriving (Eq)

type Update = M.Map LValue Expr

data LValue = IndexedRef Ref Expr
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
    mempty = RTL mempty mempty mempty
    mappend (RTL xd xc xb) (RTL yd yc yb) =
        RTL (xd <> yd) (xc <> yc) (M.unionWith (<>) xb yb)

instance Monoid Block where
    mempty = Block M.empty M.empty
    mappend (Block xc xu) (Block yc yu) =
        Block (M.unionWith (<>) xc yc) (xu <> yu)


$( makeLenses ''RTL )
$( makeLenses ''Block )
