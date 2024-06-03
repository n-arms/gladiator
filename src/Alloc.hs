module Alloc (
  
) where

import IR
import Program(Name)
import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict(HashMap)
import qualified Data.HashSet as HashSet
import Data.HashSet(HashSet)
import Lower(index)
import Control.Monad.Trans.State.Strict
import Data.Functor
import Data.List
import Data.Hashable

type BlockAlloc reg = [reg]
type FunctionAlloc reg = [reg]
data ProgramAlloc reg = ProgramAlloc (HashMap Name (FunctionAlloc reg))

data LiveRange = LiveRange Int Int

instance Semigroup LiveRange where
  (LiveRange start end) <> (LiveRange start' end') = LiveRange (start `min` start') (end `min` end')

instance Monoid LiveRange where
  mempty = LiveRange maxBound minBound

data LiveRanges = LiveRanges (HashMap Id LiveRange)

instance Semigroup LiveRanges where
  (LiveRanges ranges) <> (LiveRanges ranges') = LiveRanges $ HashMap.unionWith (<>) ranges ranges'

instance Monoid LiveRanges where
  mempty = LiveRanges HashMap.empty

liveRange :: Int -> Int -> Id -> LiveRanges
liveRange start end id = LiveRanges $ HashMap.singleton id $ LiveRange start end

startRange :: Int -> Id -> LiveRanges
startRange start id = LiveRanges $ HashMap.singleton id $ LiveRange start minBound

endRange :: Int -> Id -> LiveRanges
endRange end id = LiveRanges $ HashMap.singleton id $ LiveRange maxBound end

findLiveRanges :: Block -> LiveRanges
findLiveRanges (Block args stmts result) = let
  argRanges = mconcat $ [0..(length args) - 1] <&> \arg -> liveRange 0 0 arg
  stmtRanges = mconcat $ (index stmts) <&> \(stmt, index) -> stmtLiveRanges index stmt
  resultRanges = resultLiveRanges (length args) result in
  argRanges <> stmtRanges <> resultRanges

stmtLiveRanges :: Int -> Statement -> LiveRanges
stmtLiveRanges index (Let name _ expr) = (startRange index name) <> (mconcat $ (exprVariables expr) <&> endRange index)
   
exprVariables :: Expr -> [Id]
exprVariables (Atom atom) = atomVariables atom
exprVariables (Primitive _ args) = args >>= atomVariables
exprVariables (Call _ args) = args >>= atomVariables

atomVariables :: Atom -> [Id]
atomVariables (Variable var) = [var]
atomVariables (Literal _) = []

resultLiveRanges :: Int -> Result -> LiveRanges
resultLiveRanges index (Return result) = mconcat $ (atomVariables result) <&> endRange index 
resultLiveRanges index (Jump j) = blockJumpLiveRanges index j
resultLiveRanges index (JumpIf p j1 j2) = (blockJumpLiveRanges index j1) <> (blockJumpLiveRanges index j2)

blockJumpLiveRanges :: Int -> BlockJump -> LiveRanges
blockJumpLiveRanges index (BlockJump _ args) = mconcat $ do
  arg <- args
  var <- (atomVariables arg)
  return $ endRange index var

type Alloc reg a = State (Env reg) a

execAlloc :: Env reg -> Alloc reg a -> HashMap Id reg
execAlloc env a = allocations $ execState a env

data RegAlloc reg = RegAlloc reg LiveRange

data Env reg = Env {
  free :: HashSet reg,
  active :: [RegAlloc reg],
  allocations :: HashMap Id reg
}

freeUnused :: Register reg => Int -> Alloc reg ()
freeUnused index = do
  env <- get
  let activeRegs = active env
  let toFree = filter (\(RegAlloc _ (LiveRange _ end)) -> end <= index) activeRegs <&> \(RegAlloc reg _) -> reg
  let stillActive = filter (\(RegAlloc _ (LiveRange _ end)) -> end > index) activeRegs
  put $ env { 
    free = (free env) `HashSet.union` (HashSet.fromList toFree),
    active = stillActive
  }

allocateReg :: Register reg => Id -> LiveRange -> Alloc reg ()
allocateReg id range = do
  env <- get
  let reg = head $ HashSet.toList $ free env
  put $ env {
    free = HashSet.delete reg $ free env,
    active = (RegAlloc reg range):(active env),
    allocations = HashMap.insert id reg $ allocations env 
  }

class Hashable reg => Register reg where
  allRegisters :: HashSet reg

allocateBlock :: Register reg => HashMap Id reg -> Block -> BlockAlloc reg
allocateBlock argAllocs block@(Block args stmts result) = let
  LiveRanges liveRanges = findLiveRanges block
  orderedRanges = sortOn (\(_, (LiveRange start _)) -> start) $ HashMap.toList liveRanges
  startEnv = Env {
    free = allRegisters `HashSet.difference` (HashSet.fromList $ HashMap.elems argAllocs),
    active = [0..(length args) - 1] <&> \arg -> RegAlloc (argAllocs HashMap.! arg) (liveRanges HashMap.! arg),
    allocations = argAllocs
  }
  finalEnv = execAlloc startEnv $ allocateBlockIn 0 liveRanges stmts
  allocs = allocations finalEnv
  in [0..(length allocs) - 1] <&> \id -> allocs HashMap.! id

allocateBlockIn :: Register reg => Int -> HashMap Id LiveRange -> [Statement] -> Alloc reg ()
allocateBlockIn index ranges [] = return ()
allocateBlockIn index ranges (Let name _ expr:rest) = do
  freeUnused index
  allocateReg name $ ranges HashMap.! name
  allocateBlockIn (index + 1) ranges rest

allocateFunction :: Register reg => Function -> HashMap Id reg -> [BlockAlloc reg]
allocateFunction (Function name blocks) argRegs = let
  blocks <&> \block -> execAlloc