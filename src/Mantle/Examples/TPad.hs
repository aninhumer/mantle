{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RebindableSyntax #-}

module Mantle.Examples.TPad where

import Mantle.Prelude

import Mantle.Vector
import Mantle.Vector.Lits
import Mantle.Interface
import Mantle.Circuit

data TPad = TPad {
    ledsRed   :: Vec D18 (Input Bool),
    ledsGreen :: Vec D8  (Input Bool),
    switches  :: Vec D18 (Output Bool),
    keys      :: Vec D4  (Output Bool)
}

tpad :: MonadCircuit mc => mc (TPad)
tpad = do
    ledr <- extOutputVec "LEDR"
    ledg <- extOutputVec "LEDG"
    sw   <- extInputVec  "SW"
    key  <- extInputVec  "KEY"
    return $ TPad ledr ledg sw key
