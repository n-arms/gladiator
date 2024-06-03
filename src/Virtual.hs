{-# LANGUAGE PatternSynonyms #-}
module Virtual (
  Function, pattern VCode.Function,
  Block, pattern VCode.Block,
  Instr, pattern VCode.Move, pattern VCode.MoveAbs, pattern VCode.Push, pattern VCode.Pop, pattern VCode.Return, pattern VCode.Call, pattern VCode.FAdd, pattern VCode.FSub
) where

import qualified VCode as VCode

type Function = VCode.Function VCode.VReg
type Block = VCode.Block VCode.VReg
type Instr = VCode.Instr VCode.VReg