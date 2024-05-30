{-# LANGUAGE PatternSynonyms #-}

module Typed (
  Program, pattern Program.Program,
  Argument, pattern Program.Argument,
  Function, pattern Program.Function,
  Expr, pattern Program.Variable, pattern Program.Block, pattern Program.Literal, pattern Program.Call, pattern Program.Primitive, pattern Program.If,
  Statement, pattern Program.Let,
  getType,
  intType, boolType
) where

import qualified Program
import Program(Type(..), CallType(..), Literal(..), Primitive(..))

type Program = Program.Program Type
type Argument = Program.Argument Type
type Function = Program.Function Type
type Expr = Program.Expr Type
type Statement = Program.Statement Type


contains :: Eq a => a -> [a] -> Bool
contains elem (x:xs) = (x==elem) || contains elem xs
contains elem [] = False

getType :: Expr -> Type
getType expr = case expr of
  Program.Variable _ t -> t
  Program.Block _ (Just t) -> getType t
  Program.Literal (Number _) -> intType
  Program.Call _ _ (CType _ result) -> result
  Program.Primitive primitive _ -> if contains primitive [Plus, Minus, Times, Div]
    then intType
    else boolType
  Program.If _ _ result -> getType result
  
intType :: Type
intType = Named "Int"

boolType :: Type
boolType = Named "Bool"
