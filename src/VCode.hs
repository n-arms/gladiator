{-# LANGUAGE TypeFamilies #-}

module VCode (
  Function(..),
  Block(..),
  Instr(..),
  VReg(..),
  Res(..),
  Arg(..),
  Fun(..)
) where

import Program(Name, Literal)
import IR(Id)

data Function r = Function Name [Block r]

data Block r = Block [[Res r]] [Instr r]

data Instr r
  = Move (Res r) (Arg r)
  | MoveAbs (Res r) Literal
  | Push (Arg r)
  | Pop (Arg r)
  | Return (Arg r)
  | Call (Fun r) (Arg r) [Arg r]
  | FAdd (Res r) (Arg r)
  | FSub (Res r) (Arg r)

data VReg = VReg Id

data family Res r
data instance Res VReg = ResV VReg

data family Arg r
data instance Arg VReg = ArgV VReg

data family Fun r
data instance Fun VReg = FunV Name
