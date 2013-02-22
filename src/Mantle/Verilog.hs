{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Mantle.Verilog where

import Data.Foldable
import qualified Data.Map as M
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

genVar :: Doc -> Ref -> VType -> Doc
genVar d r w =
    d <+> genRepr w <+> genRef r <> ";"

genInputs :: M.Map Ref VType -> Doc
genInputs = genMap (genVar "input")

genOutputs :: M.Map Ref VType -> Doc
genOutputs = genMap (genVar "output")

genWires :: M.Map Ref VType -> Doc
genWires = genMap (genVar "wire")

genRegs :: M.Map Ref VType -> Doc
genRegs = genMap (genVar "reg")

genCombs cs =
    "always begin" <$>
        indent 4 (genMap genComb cs) <$>
    "end"
  where
    genComb r e =
        genRef r <+> "=" <+> genExpr e <> ";"

genRepr :: VType -> Doc
genRepr (BitType n) = "[" <> int (n-1) <> ":0]"
genRepr (VecType n r) = "[" <> int (n-1) <> ":0]" <> genRepr r

genRef :: Ref -> Doc
genRef (Ref r) = "a" <> dshow (show r)

genRegRef :: RegRef -> Doc
genRegRef (NRef r) = genRef r
genRegRef (IRef r i) = genRef r <> brackets (genExpr i)

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
    genWrite r e = genRegRef r <+> "<=" <+> genExpr e <> ";"

genExpr :: Expr -> Doc
genExpr (Lit bv) = genLiteral bv
genExpr (Var r) = genRef r
genExpr (BinOp x op y) =
    parens $ genExpr x <+> genBinOp op <+> genExpr y
genExpr (UnOp op x) =
    parens $ genUnOp op <> genExpr x
genExpr (CondE c t e) =
    parens $ genExpr c <+> "?" <+> genExpr t <+> ":" <+> genExpr e
genExpr (Index n i) =
    genRef n <> brackets (genExpr i)
genExpr (Concat es) =
    encloseSep "{" "}" "," $ map genExpr es

genLiteral :: Value -> Doc
genLiteral (Dec x) = "'d" <> integer x -- TODO: Need exact width here.
genLiteral Undef = "'x"

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
