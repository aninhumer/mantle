{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}

module Mantle.Logic where

import Mantle.Prelude
import qualified Prelude as P

import Data.Bits
import Data.Bits.Bool
import Data.Vector.Bit
import Control.Monad.Writer
import Control.Lens

import Mantle.RTL
import Mantle.Circuit
import Mantle.Interface


class Readable r a | r -> a where
    read :: r -> Output a

instance Bits a => Readable (Wire a) a where
    read = Output . Var . wireVar

instance Bits a => Readable (Reg a) a where
    read = Output . Var . regVar


infix 1 =:
class Bindable b a | b -> a where
    (=:) :: (Readable r a, MonadCircuit c) => b -> r -> c ()

instance Bits a => Bindable (Wire a) a where
    (Wire w :: Wire a) =: e = circuit $ do
        tell $ (combs.at w ?~ readExpr e) mempty

instance Bits a => Bindable (Input a) a where
    (Input w) =: e = circuit $ do
        tell $ (combs.at w ?~ readExpr e) mempty

infix 1 <=:
class Writable w a | w -> a where
    (<=:) :: (Readable r a) => w -> r -> Statement

instance Bits a => Writable (Reg a) a where
    (Reg r :: Reg a) <=: e = do
        tell $ (writes.at r ?~ readExpr e) mempty

instance Bits a => Readable (Output a) a where
    read = id

readExpr :: Readable r a => r -> Expr
readExpr = unOutput . read


newtype Constant a = Constant a

instance Bits a => Readable (Constant a) a where
    read (Constant x) = Output $ Lit $ unpack x

fromInteger :: Integral a => Integer -> Constant a
fromInteger = Constant . P.fromIntegral


literal :: Bits a => a -> Output a
literal x = Output $ Lit (unpack x)

unOp :: Readable r a => UnaryOperator -> r -> Output a
unOp op x = Output $ UnOp op (readExpr x)

binOp :: (Readable r1 a, Readable r2 a) =>
    BinaryOperator -> r1 -> r2 -> Output b
binOp op x y =
    Output $ BinOp (readExpr x) op (readExpr y)


true, false :: Output Bool
true  = literal True
false = literal False

not :: Readable r Bool => r -> Output Bool
not = unOp OpNot

(&&), (||) :: (Readable r1 Bool, Readable r2 Bool) =>
    r1 -> r2 -> Output Bool
(&&) = binOp OpAnd
(||) = binOp OpOr

ifThenElse :: (Readable r1 Bool, Readable r2 a, Readable r3 a) =>
    r1 -> r2 -> r3 -> Output a
ifThenElse c x y =
    Output $ CondE (readExpr c) (readExpr x) (readExpr y)

(==), (!=) :: (Readable r1 a, Readable r2 a, Eq a) =>
    r1 -> r2 -> Output Bool
(==) = binOp OpEqual
(!=) = binOp OpNotEq

(<), (>), (<=), (>=) ::
    (Readable r1 a, Readable r2 a, Ord a) =>
    r1 -> r2 -> Output Bool
(<) = binOp OpLT
(>) = binOp OpGT
(<=) = binOp OpLTE
(>=) = binOp OpGTE


class Arith a where
    (+), (-), (*), (/), (%) ::
        (Readable r1 a, Readable r2 a) => r1 -> r2 -> Output a
    (+) = binOp OpAdd
    (-) = binOp OpSub
    (*) = binOp OpMul
    (/) = binOp OpDiv
    (%) = binOp OpMod

instance Arith Int where

comb :: (Bits a, Readable r a) => r -> Circuit (Output a)
comb x = do
    w <- newWire
    w =: x
    return $ read w

iff :: Readable c Bool => c -> Statement -> Statement
iff cond stmt = do
    let (_,blk) = runWriter stmt
    let c = readExpr cond
    tell $ (conds.at c ?~ blk) mempty
