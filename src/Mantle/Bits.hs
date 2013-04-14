{-# LANGUAGE ScopedTypeVariables #-}

module Mantle.Bits where

import qualified Data.Vector.Fixed as V
import Data.Vector.Fixed.Boxed (Vec)

import Mantle.RTL

class Bits a where
    repType :: a -> VType
    repExpr :: a -> Expr

instance Bits Bool where
    repType _ = BitType
    repExpr True  = Lit $ Dec 1
    repExpr False = Lit $ Dec 0

instance Bits Int where
    -- TODO: Replace with explicit IntNs
    repType _ = VecType 64 BitType
    repExpr = Lit . Dec . toInteger

instance (V.Arity n, Bits a) => Bits (Vec n a) where
    repType _ = VecType nval arep
      where
        nval = V.length (undefined :: Vec n a)
        arep = repType  (undefined :: a)
    repExpr v = Concat $ map repExpr $ V.toList v
