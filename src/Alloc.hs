{-# LANGUAGE MultiParamTypeClasses #-}

module Alloc (
  
) where

import IR
import Program(Name)
import Live
import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict(HashMap)
import qualified Data.HashSet as HashSet
import Data.HashSet(HashSet)
import Lower(index)
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Writer.Strict
import Control.Monad.Trans.Class
import Data.Functor
import Data.List
import Data.Hashable

{-
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
-}

data Env reg = Env {
  -- mandatoryAllocs[line] = (i, r) means that on line `line`, id `i` *must* be bound to register `r`
  mandatoryAllocs :: HashMap Int (Id, reg),
  -- the live ranges of all ids
  liveRanges :: LiveRanges,
  -- the set of currently free registers
  free :: [reg],
  -- active = (id, reg):rest means that currently id `id` is alloated to register `reg`
  active :: [(Id, Location reg)],
  -- the number of spills that have occured
  spillCount :: StackSlot
}

data Location reg = InRegister reg | InStack StackSlot

-- this DOES NOT emit an allocation instruction, as you could be calling it to unspill a register or store a new one
-- you need to emit an allocation instruction yourself
allocateReg :: Line -> Id -> [Id] -> Alloc reg reg
allocateReg line id noSpill = do
  env <- get
  case free env of
    reg:rest -> do {
      put $ env { free = rest };
      allocateIdIntoReg line id reg
    }
    [] -> spillThenAlloc line id noSpill

allocateIdIntoReg :: Line -> Id -> reg -> Alloc reg ()
allocateIdIntoReg line id reg = do
  env <- get
  put $ env { active = (id, Register reg):(active env) }

lookupLiveRange :: Id -> Alloc reg LiveRange
lookupLiveRange id = do
  env <- get
  return $ (liveRanges env) HashMap.! id

lastEnding :: [Id] -> Alloc reg ActiveAlloc
lastEnding noSpill = do
  env <- get
  liveRanges <- sequence $ (active env) <&> (lookupLiveRange . fst)
  let last = foldl1' (oldest noSpill) $ zip (liveRanges <&> rangeEnd) $ active env
  where
    oldest noSpill (end, res) (end', res') =
      if end > end'
        then (end, res)
        else (end', res')

{-
lastEnding :: [Id] -> Alloc reg ActiveAlloc
lastEnding noSpill = do
  env <- get
  lastEnding' noSpill 0 0 0 $ active env
  where
    lastEnding' noSpill index best bestEnd ((id, InRegister reg):rest) = do
      LiveRange _ end <- lookupLiveRange id
      if notElem id noSpill && end > bestEnd
        then lastEnding' (index + 1) index end rest
        else lastEnding' (index + 1) best bestEnd rest
    lastEnding' noSpill index best bestEnd ((_, InStack _):rest) = lastEnding' noSpill (index + 1) best bestEnd rest
    lastEnding' _ _ best _ [] = return best
-}
popActive :: ActiveAlloc -> Alloc reg (Id, reg)
popActive index = do
  env <- get
  let (result, newActive) = pop index $ active env
  put $ env { active = newActive }
  return result
  where
    pop 0 (result:rest) = case result of 
      (id, InRegister reg) -> ((id, reg), rest)
    pop n (head:rest) = 
      let (result, rest') = pop (n - 1) rest in
      (result, head:rest')

registerInstr :: Line -> Id -> reg -> Alloc reg ()
registerInstr line id reg = do
  lift $ tell [Register line id reg]

spillInstr :: Line -> reg -> StackSlot -> Alloc reg ()
spillInstr line reg slot = do
  lift $ tell [Spill line reg slot]

unSpillInstr :: Line -> reg -> StackSlot -> Alloc reg ()
unSpillInstr line id slot = do
  lift $ tell [UnSpill line id slot]

spillSlot :: Alloc reg StackSlot
spillSlot = do
  env <- get
  let slot = spillCount env
  put $ env { spillCount = slot + 1 }
  return $ slot

-- spill any id that isn't in the noSpill list in order to free up a register
-- and allocate `id`
spillThenAlloc :: Line -> Id -> [Id] -> Alloc reg ()
spillThenAlloc line id noSpill = do
  toSpill <- lastEnding noSpill
  (oldId, reg) <- popActive toSpill
  slot <- spillSlot
  spillInstr line reg slot
  env <- get
  put $ env {
    active = (id, InRegister reg):(oldId, InStack slot):(active env)
  }

freeUnused :: Line -> Alloc reg ()
freeUnused line = do
  env <- get
  let activeList = active env
  keepList <- sequence $ activeList <&> \(id, _) -> 
    do
      LiveRange _ end <- lookupLiveRange id
      return $ end > line
  let newActive = map snd $ filter fst $ zip keepList activeList
  put $ env { active = newActive }

allocateBlock :: Instruction i => Line -> [i] -> Alloc reg ()
allocateBlock line [] = return ()
allocateBlock line (instr:rest) = do
  let used = uses instr
  sequence $ used <&> \id -> unspill line id used

  freeUnused line

  let id = defines instr
  reg <- allocateReg line id []
  registerInstr line id reg
  
  allocateBlock (line + 1) rest

unspill :: Line -> Id -> [Id] -> Alloc reg ()
unspill line id noSpill = do
  env <- get
  let (_, location) = head $ filter (((==) id) . fst) $ active env
  case location of
    InRegister _ -> return ()
    InStack slot -> unspillFrom line id noSpill slot
  where
    unspillFrom line id noSpill slot = do
      reg <- allocateReg line id noSpill
      unSpillInstr line reg slot

      

type AllocList reg = [AllocInstr reg]
data AllocInstr reg = Register Line Id reg | Spill Line reg StackSlot | UnSpill Line reg StackSlot

type Alloc reg a = StateT (Env reg) (Writer (AllocList reg)) a

class Instruction instr where
  defines :: instr -> Id
  uses :: instr -> [Id]
