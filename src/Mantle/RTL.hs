{-# LANGUAGE TemplateHaskell #-}

module Mantle.RTL where

import Control.Lens.TH
import Data.Vector.Bit
import Data.String
import qualified Data.Sequence as S
import qualified Data.Map as M
import qualified Data.Set as Set


data RTL = RTL {
    _vars   :: M.Map Name Variable,
    _blocks :: M.Map Trigger Block
}

newtype Name = Name { name :: String }
    deriving (Eq, Ord)

instance Show Name where
    show = name

instance IsString Name where
    fromString = Name

data Variable = Variable {
    _varType :: VarType,
    _width   :: Int
}

data VarType = WireVar | RegVar

type Trigger = Set.Set Edge

data Edge = PosEdge Name
          | NegEdge Name
          | EitherEdge Name
          deriving (Eq,Ord)

type Block = S.Seq Statement

data Statement = Cond Expr [Statement] [Statement]
            | BlockingAssign Name Expr
            | AsyncAssign Name Expr

data Expr = Lit BitVector
          | Var Name
          | BinOp Expr BinaryOperator Expr
          | UnOp UnaryOperator Expr
          | CondE Expr Expr Expr
          | BitSel Name Expr
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
