{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Mantle.Verilog where

import Data.Foldable
import qualified Data.Map as M
import qualified Data.Vector.Unboxed as V
import qualified Data.Set as Set
import Data.Text.Lazy hiding (map)
import Text.PrettyPrint.Leijen.Text

import Mantle.RTL
import Mantle.Circuit
import Mantle.Interface

dshow :: String -> Doc
dshow = text.pack

genMap :: (a -> b -> Doc) -> M.Map a b -> Doc
genMap f m = vcat $ map (uncurry f) $ M.assocs m

genModule :: String -> RTL -> Doc
genModule name rtl@(RTL is os _ _ _ _) =
    "module" <+> dshow name <+> tupled ioNames <> ";" <$>
        indent 4 (genRTL rtl) <$>
    "endmodule"
  where
    ioNames = map genRef $ (M.keys is) ++ (M.keys os)

genComponent :: forall ifc. Interface (FlipIfc ifc) =>
    String -> Component Circuit ifc -> Doc
genComponent name comp =
    genModule name (buildCircuit (makeExtern comp :: Circuit (VoidIfc ifc)))

genRTL :: RTL -> Doc
genRTL (RTL is os ws rs cs bs) =
    genInputs is <$>
    genOutputs os <$>
    genWires ws <$>
    genRegs rs <$>
    genCombs cs <$>
    genBlocks bs

genVar :: Doc -> Ref -> Width -> Doc
genVar d r w =
    d <+> genWidth w <+> genRef r <> ";"

genInputs :: M.Map Ref Width -> Doc
genInputs = genMap (genVar "input")

genOutputs :: M.Map Ref Width -> Doc
genOutputs = genMap (genVar "output")

genWires :: M.Map Ref Width -> Doc
genWires = genMap (genVar "wire")

genRegs :: M.Map Ref Width -> Doc
genRegs = genMap (genVar "reg")

genCombs cs =
    "always begin" <$>
        indent 4 (genMap genComb cs) <$>
    "end"
  where
    genComb r e =
        genRef r <+> "=" <+> genExpr e <> ";"

genWidth 1 = ""
genWidth n = "[" <> int (n-1) <> ":0]"

genRef :: Ref -> Doc
genRef (Ref r) = "a" <> dshow (show r)

genIRef :: IRef -> Doc
genIRef (NRef r) = genRef r
genIRef (IRef i r) = genRef r <> brackets (genRef i)

genBlocks :: M.Map Trigger Block -> Doc
genBlocks =
    genMap genTrigBlock
  where
    genTrigBlock t b =
        "always @" <> genTrigger t <+> genBlock b
    genTrigger =
        encloseSep "(" ")" "," . map genEdge . toList
    genEdge v = case v of
        PosEdge p -> "posedge" <+> genRef p
        NegEdge n -> "negedge" <+> genRef n
        EitherEdge e -> genRef e

genBlock :: Block -> Doc
genBlock (Block cs ws) =
    "begin" <$>
        indent 4 (genConds cs <$> genWrites ws) <$>
    "end"
  where

genConds :: M.Map Expr Block -> Doc
genConds =
    genMap genCond
  where
    genCond c b =
        "if (" <> genExpr c <> ")" <+> genBlock b

genWrites :: Update -> Doc
genWrites =
    genMap genWrite
  where
    genWrite r e = genIRef r <+> "<=" <+> genExpr e <> ";"

genExpr :: Expr -> Doc
genExpr (Lit bv) = genLiteral bv
genExpr (Var r) = genRef r
genExpr (BinOp x op y) =
    parens $ genExpr x <+> genBinOp op <+> genExpr y
genExpr (UnOp op x) =
    parens $ genUnOp op <> genExpr x
genExpr (CondE c t e) =
    parens $ genExpr c <+> "?" <+> genExpr t <+> ":" <+> genExpr e
genExpr (BitSel n i) =
    genRef n <> brackets (genExpr i)
genExpr (Concat es) =
    encloseSep "{" "}" "," $ map genExpr es

genLiteral :: V.Vector Bool -> Doc
genLiteral bv
    | V.length bv == 0 = "'b0"
    | otherwise        = "'b" <> bits
  where
    bits = foldMap genBit $ V.toList $ V.reverse bv
    genBit True  = "1" 
    genBit False = "0" 

genBinOp :: BinaryOperator -> Doc
genBinOp op = case op of
    OpAdd -> "+"
    OpSub -> "-"
    OpMul -> "*"
    OpDiv -> "/"
    OpMod -> "%"
    OpAnd -> "&&"
    OpOr  -> "||"
    OpEqual -> "=="
    OpNotEq -> "!="
    OpLT  -> "<"
    OpGT  -> ">"
    OpLTE -> "<="
    OpGTE -> ">="
    OpShiftL -> "<<"
    OpShiftR -> ">>"
    OpBitAnd -> "&"
    OpBitOr  -> "|"
    OpBitXor -> "^"

genUnOp :: UnaryOperator -> Doc
genUnOp op = case op of
    OpNegate -> "~"
    OpNot -> "!"
