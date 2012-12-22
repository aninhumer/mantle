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

genRTL :: RTL -> Doc
genRTL (RTL vs bs) =
    genDecls vs <> line <$> genActions bs

genDecls :: M.Map Ref Variable -> Doc
genDecls vs = vcat $ map (uncurry genDecl) $ M.assocs vs

genDecl :: Ref -> Variable -> Doc
genDecl n (Variable vt w) =
    keyword vt <+> sizeDef w <+> dshow n <> ";"
  where
    keyword WireVar = "wire"
    keyword RegVar  = "reg"
    sizeDef 1 = ""
    sizeDef n = "[" <> int (n-1) <> ":0]"

genActions :: M.Map Trigger Block -> Doc
genActions as = vcat $ map (uncurry genAlways) $ M.assocs as

genAlways :: Trigger -> Block -> Doc
genAlways t b
    | Set.null t = "always" <+> genBlock b
    | otherwise  = "always @" <> genTrigger t <+> genBlock b

genTrigger :: Trigger -> Doc
genTrigger = encloseSep "(" ")" "," . map genEdge . toList

genEdge :: Edge -> Doc
genEdge (PosEdge v) = "posedge" <+> dshow v
genEdge (NegEdge v) = "negedge" <+> dshow v
genEdge (EitherEdge v) = dshow v

genBlock :: Block -> Doc
genBlock blk = "begin" <$> indent 4 stmts <$> "end"
  where
    stmts = vcat $ toList $ fmap genStmt blk

genStmt :: Statement -> Doc
genStmt (Cond c t e) =
    "if" <+> parens (genExpr c) <+> genBlock t <+> "else" <+> genBlock e
genStmt (BlockingAssign n e) =
    dshow n <+> "=" <+> genExpr e <> ";"
genStmt (AsyncAssign n e) =
    dshow n <+> "<=" <+> genExpr e <> ";"

genExpr :: Expr -> Doc
genExpr (Lit bv) = genLiteral bv
genExpr (Var n) = dshow n
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
