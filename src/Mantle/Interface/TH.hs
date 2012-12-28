{-# LANGUAGE TemplateHaskell #-}

module Mantle.Interface.TH where

import Control.Monad
import Control.Lens
import Language.Haskell.TH
import Language.Haskell.TH.Lens
import Language.Haskell.TH.Build hiding (name)
import Mantle.Interface


appsT :: TypeQ -> [TypeQ] -> TypeQ
appsT = foldl appT

unTV (PlainTV t) = return $ VarT t
unTV (KindedTV t k) = return $ SigT (VarT t) k

baseL :: SimpleIso Name String
baseL = iso nameBase mkName

polyType :: Name -> [TyVarBndr] -> TypeQ
polyType n tvs = appsT (conT n) $ map unTV tvs


makeInterface :: Name -> Q [Dec]
makeInterface n = do
    TyConI (DataD context ifcName tyvars [con] _) <- reify n
    ifcT <- polyType ifcName tyvars
    inst <- instanceD' context (appT' ''Interface ifcT) [
        dataIfcDef n ifcT con,
        newIfcDef n con,
        exposeDef n con]
    return [inst]

dataIfcDef :: Name -> Type -> Con -> DecQ
dataIfcDef ifcName ifcT con = do
    d <- VarT `fmap` newName "d"
    let ifcWrap = AppT $ AppT (ConT ''Ifc) d
    ifcC <- case con of
        (NormalC _ sts) ->
            let wrap (s,t) = (s,ifcWrap t) in
            normalC' ifcName $ map wrap sts
        (RecC _ vsts) ->
            let wrap (fn,s,t) = (fieldName fn,s,ifcWrap t) in
            recC' ifcName $ map wrap vsts
    dataInstD' () ''Ifc [d,ifcT] [ifcC] ()
  where
    fieldName = mkName . tail . nameBase

consCount :: Con -> Int
consCount (NormalC n sts) = length sts
consCount (RecC n vsts)   = length vsts
consCount (InfixC x n y)  = 2

newNames :: Int -> Q [Name]
newNames i = replicateM i $ newName "x"

newIfcDef :: Name -> Con -> DecQ
newIfcDef n c = do
    vars <- newNames $ consCount c
    news <- zipWithM bindS' vars (repeat 'newIfc)
    ret  <- noBindS' $ appE' 'return $ appsE' (n : vars)
    body <- doE' $ news ++ [ret]
    funD' 'newIfcCircuit $ clause' () body ()

exposeDef :: Name -> Con -> DecQ
exposeDef n c = do
    vars <- newNames $ consCount c
    body <- appsE' $ (conE' n) : (map (appE' 'expose) vars)
    funD' 'expose $ clause' (conP' n vars) body ()

