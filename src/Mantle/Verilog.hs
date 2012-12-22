{-# LANGUAGE OverloadedStrings #-}

module Mantle.Verilog where

import Data.Foldable
import qualified Data.Map as M
import qualified Data.Vector.Unboxed as V
import qualified Data.Set as Set
import Data.Text.Lazy hiding (map)
import Text.PrettyPrint.Leijen.Text

import Mantle.RTL

dshow :: Show a => a -> Doc
dshow = text.pack.show

genMap :: (a -> b -> Doc) -> M.Map a b -> Doc
genMap f m = vcat $ map (uncurry f) $ M.assocs m

genRTL :: RTL -> Doc
genRTL (RTL ws rs bs) =
    genWires ws <$> genRegs rs <$> genBlocks bs

genWires :: M.Map Ref Comb -> Doc
genWires ws =
    genMap genWire ws <$>
    "always begin" <$>
        indent 4 (genMap genComb ws) <$>
    "end"
  where
    genWire r (Comb w _) =
        "wire" <+> genWidth w <+> genRef r <> ";"
    genComb r (Comb _ e) =
        genRef r <+> "=" <+> genExpr e <> ";"

genRegs :: M.Map Ref Width -> Doc
genRegs =
    genMap genReg
  where
    genReg r w = "reg" <+> genWidth w <+> genRef r <> ";"

genWidth 1 = ""
genWidth n = "[" <> int (n-1) <> ":0]"

genRef :: Ref -> Doc
genRef (Ref r) = "a" <> dshow r

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
    genWrite r e = genRef r <+> "<=" <+> genExpr e <> ";"

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
    dshow n <> brackets (genExpr i)
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
