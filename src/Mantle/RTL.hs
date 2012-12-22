{-# LANGUAGE TemplateHaskell #-}

module Mantle.RTL where

import Control.Lens.TH
import Data.Vector.Bit
import Data.Monoid
import qualified Data.Sequence as S
import qualified Data.Map as M
import qualified Data.Set as Set


data RTL = RTL {
    _wires  :: M.Map Ref Comb,
    _regs   :: M.Map Ref Width,
    _blocks :: M.Map Trigger Block
}

newtype Ref = Ref Int
    deriving (Eq, Ord)

data Comb = Comb {
    _width :: Width,
    _comb  :: Expr
}

type Width = Int

type Trigger = Set.Set Edge

data Edge = PosEdge Ref
          | NegEdge Ref
          | EitherEdge Ref
          deriving (Eq,Ord)

data Block = Block {
    _conds  :: M.Map Expr Block,
    _writes :: Update
}

type Update = M.Map Ref Expr

data Expr = Lit BitVector
          | Var Ref
          | BinOp Expr BinaryOperator Expr
          | UnOp UnaryOperator Expr
          | CondE Expr Expr Expr
          | BitSel Ref Expr
          | BitRange Expr Int Int
          | Concat [Expr]
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
    mempty = RTL M.empty M.empty M.empty
    mappend (RTL xw xr xb) (RTL yw yr yb) =
        RTL (xw <> yw) (xr <> yr) (M.unionWith (<>) xb yb)

instance Monoid Block where
    mempty = Block M.empty M.empty
    mappend (Block xc xu) (Block yc yu) =
        Block (M.unionWith (<>) xc yc) (xu <> yu)


$( makeLenses ''RTL )
$( makeLenses ''Block )
