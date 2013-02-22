{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module Mantle.Logic where

import Mantle.Prelude
import qualified Prelude as P

import Control.Monad.Writer
import Control.Lens

import Mantle.RTL
import Mantle.Bits
import Mantle.Circuit
import Mantle.Interface


infix 1 <=:
(<=:) :: Reg a -> Output a -> Statement
Reg r <=: e = do
    tell $ (writes.at r ?~ unOutput e) mempty

rd :: Reg a -> Output a
rd (Reg ir) = Output $ case ir of
    NRef r   -> Var r
    IRef r i -> VecIndex r i

extern :: (Interface ifc, Interface (FlipIfc ifc), MonadCircuit c) =>
    ifc -> c ()
extern x = do
    o <- extIfc
    o =: x

undef :: Output a
undef = Output $ Lit Undef

literal :: Bits a => a -> Output a
literal = Output . repExpr

unOp :: UnaryOperator -> Output a -> Output a
unOp op x = Output $ UnOp op (unOutput x)

binOp :: BinaryOperator -> Output a -> Output a -> Output b
binOp op x y =
    Output $ BinOp (unOutput x) op (unOutput y)


true, false :: Output Bool
true  = literal True
false = literal False

not :: Output Bool -> Output Bool
not = unOp OpNot

(&&), (||) :: Output Bool -> Output Bool -> Output Bool
(&&) = binOp OpAnd
(||) = binOp OpOr

ifThenElse ::
    Output Bool -> Output a -> Output a -> Output a
ifThenElse c x y =
    Output $ CondE (unOutput c) (unOutput x) (unOutput y)

(==), (!=) ::
    Output a -> Output a -> Output Bool
(==) = binOp OpEqual
(!=) = binOp OpNotEq

(<), (>), (<=), (>=) :: Num a =>
    Output a -> Output a -> Output Bool
(<) = binOp OpLT
(>) = binOp OpGT
(<=) = binOp OpLTE
(>=) = binOp OpGTE


instance Num (Output Int) where
    (+) = binOp OpAdd
    (-) = binOp OpSub
    (*) = binOp OpMul
    abs x = if x > 0 then x else (-x)
    signum x =
        if x > 0 then 1 else
        if x < 0 then (-1) else 0
    fromInteger = literal . fromInteger

toWire :: forall a. Bits a => Output a -> Circuit (Wire a)
toWire x = do
    (Wire w :: Wire a) <- newWire
    (Input w) =: x
    return $ Wire w

comb :: Bits a => Output a -> Circuit (Output a)
comb x = do
    (Wire w) <- toWire x
    return $ Output (Var w)


iff :: Output Bool -> Statement -> Statement
iff cond stmt = do
    let (_,blk) = runWriter stmt
    let c = unOutput cond
    tell $ (conds.at c ?~ blk) mempty
