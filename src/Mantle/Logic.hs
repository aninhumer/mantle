{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Mantle.Logic where

import Mantle.Prelude
import qualified Prelude as P

import Data.Set
import Data.Bits
import Data.Bits.Bool
import Data.Vector.Bit
import Control.Monad.Writer
import Control.Lens

import Mantle.RTL
import Mantle.Circuit


class Bits a => Readable r a | r -> a where
    read :: r -> Expr

instance Bits a => Readable (Wire a) a where
    read = Var . wireVar

instance Bits a => Readable (Reg a) a where
    read = Var . regVar


class Bits a => Bindable b a | b -> a where
    (=:) :: (Readable r a, MonadCircuit c) => b -> r -> c ()

instance Bits a => Bindable (Wire a) a where
    (Wire w :: Wire a) =: e = circuit $ do
        tell $ (wires.at w ?~ Comb size (read e)) mempty
        where size = bitSize (undefined :: a)

class Bits a => Writable w a | w -> a where
    (<=:) :: (Readable r a) => w -> r -> Statement

instance Bits a => Writable (Reg a) a where
    (Reg r :: Reg a) <=: e = do
        tell $ (writes.at r ?~ read e) mempty

newtype Signal a = Signal Expr

instance Bits a => Readable (Signal a) a where
    read (Signal e) = e

readSignal :: Readable r a => r -> Signal a
readSignal = Signal . read

instance Boolean (Logic Bool) where
    true  = literal True
    false = literal False
    notB  = unOp OpNot
    (&&*) = binOp OpAnd
    (||*) = binOp OpOr

type instance BooleanOf (Logic a) = Logic Bool
newtype Constant a = Constant a

instance Bits a => Readable (Constant a) a where
    read (Constant x) = Lit $ unpack x

fromInteger :: Integral a => Integer -> Constant a
fromInteger = Constant . P.fromIntegral

instance IfB (Logic a) where
    ifB c x y = Logic $ CondE (expr c) (expr x) (expr y)

instance EqB (Logic a) where
    (==*) = binOp OpEqual
    (/=*) = binOp OpNotEq
literal :: Bits a => a -> Signal a
literal x = Signal $ Lit (unpack x)

unOp :: Readable r a => UnaryOperator -> r -> Signal a
unOp op x = Signal $ UnOp op (read x)

binOp :: (Readable r1 a, Readable r2 a) =>
    BinaryOperator -> r1 -> r2 -> Signal b
binOp op x y =
    Signal $ BinOp (read x) op (read y)


instance OrdB (Logic a) where
    (<*) = binOp OpLT
    (>*) = binOp OpGT
    (<=*) = binOp OpLTE
    (>=*) = binOp OpGTE

instance (Integral a, Bits a) => Num (Logic a) where
    (+) = binOp OpAdd
    (-) = binOp OpSub
    (*) = binOp OpMul
    negate = unOp OpNegate
    abs x = ifB (x > 0) x (negate x)
    signum x = ifB (x > 0) 1 (ifB (x < 0) (-1) 0)
    fromInteger = literal . fromInteger
