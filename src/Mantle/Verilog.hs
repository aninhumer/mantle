{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Mantle.Verilog where

import Data.Foldable hiding (elem)
import Data.Monoid hiding ((<>))
import qualified Data.Map as M
import qualified Data.Text.Lazy as T
import Text.PrettyPrint.Leijen.Text

import Mantle.RTL
import Mantle.Circuit
import Mantle.Interface

dshow :: String -> Doc
dshow = text . T.pack

genMap :: (a -> b -> Doc) -> M.Map a b -> Doc
genMap f m = vcat $ map (uncurry f) $ M.assocs m

genModule :: String -> RTL -> Doc
genModule name rtl@(RTL ds _ _) =
    "module" <+> dshow name <+> tupled ioNames <> ";" <$>
        indent 4 (genRTL rtl) <$>
    "endmodule"
  where
    ioNames = map (genRef . dref) ioDecls
    ioDecls = filter (flip elem [DInput,DOutput] . dtype) ds

genComponent :: String -> Circuit () -> Doc
genComponent name = genModule name . buildCircuit

genRTL :: RTL -> Doc
genRTL (RTL ds cs bs) =
    genDecls ds <$>
    genCombs cs <$>
    genBlocks bs

genDecls :: [Declaration] -> Doc
genDecls = vcat . map genDecl
  where
    genDecl (Declaration d v r) =
        genDType d <+> genVType v <+> genRef r <> ";"

genDType :: DType -> Doc
genDType DInput  = "input"
genDType DOutput = "output"
genDType DWire   = "wire"
genDType DReg    = "reg"

genCombs :: M.Map LValue Expr -> Doc
genCombs cs = (genMap genComb cs)
  where
    genComb l e =
        "assign" <+> genLValue l <+> "=" <+> genExpr e <> ";"

genVType :: VType -> Doc
genVType BitType = ""
genVType (VecType n r) = "[" <> int (n-1) <> ":0]" <> genVType r

genRef :: Ref -> Doc
genRef (Ref r)   = "a" <> dshow (show r)
genRef (Named n) = dshow n

genLValue :: LValue -> Doc
genLValue (NormalRef  r  ) = genRef r
genLValue (IndexedRef r i) = genRef r <> brackets (genExpr i)

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

genConds :: M.Map Expr (Block,Block) -> Doc
genConds =
    genMap genCond
  where
    genCond c (t,e) =
        "if (" <> genExpr c <> ")"
            <+> genBlock t <+> elseClause e
    elseClause e
        | e == mempty = ""
        | otherwise   = "else" <+> genBlock e

genWrites :: M.Map LValue Expr -> Doc
genWrites =
    genMap genWrite
  where
    genWrite l e = genLValue l <+> "<=" <+> genExpr e <> ";"

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
