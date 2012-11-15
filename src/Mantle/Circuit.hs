{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}

module Mantle.Circuit where

import Prelude hiding (length)

import Control.Monad.State
import Control.Lens
import Data.Bits
import qualified Data.Map as M
import Data.Sequence
import Data.Sequence.Lens

import Mantle.RTL
import Mantle.Logic

addWithIndex :: MonadState s c => Simple Lens s (Seq a) -> a -> c Int
addWithIndex lens x = do
    ref <- uses lens length
    lens %= (|> x)
    return ref

type Circuit c = MonadState RTL c

(|>=) :: MonadState s c => Simple Setter s (Seq a) -> a -> c ()
x |>= y = x %= (|>y)

emptyWire :: forall a c. (Circuit c, Bits a) => c (Logic a)
emptyWire = do
    ref <- wires `addWithIndex` Wire size Nothing
    return $ variable ref
  where
    size = bitSize (undefined :: a)

wire :: forall a c. (Circuit c, Bits a) => Logic a -> c (Logic a)
wire (Logic e) = do
    ref <- wires `addWithIndex` Wire size (Just e)
    return $ variable ref
  where
    size = bitSize (undefined :: a)

newtype Reg a = Reg RegRef

register :: forall a c. (Circuit c, Bits a) => c (Reg a)
register = do
    ref <- registers `addWithIndex` size
    return $ Reg ref
  where
    size = bitSize (undefined :: a)

latch :: Circuit c => Logic Bool -> c (LatchRef)
latch (Logic cond) = do
    latches `addWithIndex` Latch cond M.empty

sync :: Circuit c => Clock -> Reset -> c (SyncRef)
sync clk rst = do
    syncs `addWithIndex` Sync clk M.empty rst M.empty

rd :: Reg a -> Logic a
rd (Reg r) = Logic $ Acc r
