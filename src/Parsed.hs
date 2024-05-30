{-# LANGUAGE PatternSynonyms #-}

module Parsed (
  Program, pattern Program.Program,
  Argument, pattern Program.Argument,
  Function, pattern Program.Function,
  Expr, pattern Program.Variable, pattern Program.Block, pattern Program.Literal, pattern Program.Call, pattern Program.Primitive, pattern Program.If,
  Statement, pattern Program.Let,
) where

import qualified Program

type Program = Program.Program ()
type Argument = Program.Argument ()
type Function = Program.Function ()
type Expr = Program.Expr ()
type Statement = Program.Statement ()
