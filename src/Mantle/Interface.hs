{-# LANGUAGE ScopedTypeVariables #-}

module Mantle.Interface where

import Data.Bits

import Mantle.RTL
import Mantle.Circuit

data Direction = Internal | External

class Interface ifc where
    newIfc :: Direction -> Circuit ifc
    setDir :: Direction -> ifc -> ifc

data Input a = Input {
    inputWire :: Wire a,
    inputDir  :: Direction
}

instance Bits a => Interface (Input a) where
    newIfc d = do
        w <- freshWire
        return $ Input w d
    setDir d (Input x _) = Input x d

data Output a = Output {
    outputWire :: Wire a,
    outputDir  :: Direction
}

instance Bits a => Interface (Output a) where
    newIfc d = do
        w <- freshWire
        return $ Output w d
    setDir d (Output x _) = Output x d

instance (Interface a, Interface b) => Interface (a,b) where
    newIfc d = do
        x <- newIfc d
        y <- newIfc d
        return (x,y)
    setDir d (x,y) = (setDir d x, setDir d y)

type Component ifc = ifc -> Circuit ()

make :: Interface ifc => Component ifc -> Circuit ifc
make cf = do
    ifc <- newIfc External
    cf ifc
    return $ setDir Internal ifc