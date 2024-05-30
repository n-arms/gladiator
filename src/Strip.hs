{-# LANGUAGE BlockArguments #-}

module Strip (
  stripProgram
) where

import IR
import Data.Functor
import Data.HashSet(HashSet)
import qualified Data.HashSet as HashSet
import Data.HashMap.Strict(HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe(fromMaybe)
import Control.Monad.Trans.State.Strict
import Lower(index)
import Debug.Trace

data ArgumentUsage = ArgumentUsage (HashSet Id)

instance Semigroup ArgumentUsage where
  (ArgumentUsage left) <> (ArgumentUsage right) = ArgumentUsage $ HashSet.union left right

instance Monoid ArgumentUsage where
  mempty = ArgumentUsage $ HashSet.empty

useId :: Id -> ArgumentUsage
useId = ArgumentUsage . HashSet.singleton

identifyBlock :: Block -> ArgumentUsage
identifyBlock (Block _ statements result) =
  (mconcat $ statements <&> identifyStatement) <> (identifyResult result)

identifyStatement :: Statement -> ArgumentUsage
identifyStatement (Let _ _ expr) = identifyExpr expr

identifyExpr :: Expr -> ArgumentUsage
identifyExpr (Atom atom) = identifyAtom atom
identifyExpr (Primitive _ args) = mconcat $ args <&> identifyAtom
identifyExpr (Call _ args) = mconcat $ args <&> identifyAtom

identifyAtom :: Atom -> ArgumentUsage
identifyAtom (Variable var) = useId var
identifyAtom (Literal _) = mempty

identifyResult :: Result -> ArgumentUsage
identifyResult (Return atom) = identifyAtom atom
identifyResult (Jump (BlockJump _ args)) = mconcat $ args <&> identifyAtom
identifyResult (JumpIf p (BlockJump _ args) (BlockJump _ args')) = mconcat $ (args <> args' <> [p]) <&> identifyAtom


type BlockTree = HashMap Id BlockInfo

data BlockInfo = BlockInfo Block (Maybe [Id]) deriving Show

type Tree a = State BlockTree a

execTree :: BlockTree -> Tree a -> BlockTree
execTree = flip execState

lookupBlock :: Id -> Tree Block
lookupBlock id = do
  tree <- get
  let BlockInfo block _ = tree HashMap.! id
  return block

lookupRemap :: Id -> Tree (Maybe [Id])
lookupRemap id = do
  tree <- get
  let BlockInfo _ remap = tree HashMap.! id
  return remap

setBlock :: Id -> BlockInfo -> Tree ()
setBlock id block = do
  tree <- get
  put $ HashMap.insert id block tree

stripProgram :: Program -> Program
stripProgram (Program funcs) = Program $ funcs <&> stripFunction

stripFunction :: Function -> Function
stripFunction (Function name blocks) = let
  initialTree = HashMap.fromList $ zip [0..(length blocks) - 1] $ blocks <&> \block -> BlockInfo block Nothing
  blockTree = execTree initialTree $ do
    Block args stmts oldResult <- lookupBlock 0
    result <- stripResult oldResult
    setBlock 0 $ BlockInfo (Block args stmts result) $ Just [0..(length args) - 1]
  newBlocks = [0..(length blocks) - 1] <&> \blockId -> 
    let BlockInfo block _ = blockTree HashMap.! blockId in block in
  Function name newBlocks

-- the ids of the blocks immediatly reachable from the given block id
nextBlocks :: Id -> Tree [Id]
nextBlocks blockId = do
  Block _ _ result <- lookupBlock blockId
  return $ case result of
    Return _ -> []
    Jump (BlockJump target _) -> [target]
    JumpIf _ (BlockJump t1 _) (BlockJump t2 _) -> [t1, t2]

-- returns a list containing the identifiers that this function actually accepts as arguments
stripBlock :: Id -> Tree [Id]
stripBlock blockId = do
  existingRemap <- lookupRemap blockId
  case existingRemap of
    Just remap -> return remap
    Nothing -> do
      (Block args stmts result) <- lookupBlock blockId
      newResult <- stripResult result
      let block = Block args stmts result
      let (ArgumentUsage used) = identifyBlock $ Block args stmts newResult
      let (newArgs, remapIndices) = unzip $ do {
          (arg, index) <- index args;
          if HashSet.member index used
            then return (arg, index)
            else []  
      }
      let spec = HashMap.fromList $ index remapIndices
      let newStmts = stmts <&> apply spec
      let finalResult = apply spec newResult
      setBlock blockId $ BlockInfo (Block newArgs newStmts finalResult) $ Just remapIndices
      return remapIndices  

stripResult :: Result -> Tree Result
stripResult (Return atom) = return $ Return atom
stripResult (Jump j) = do
  j' <- stripBlockJump j
  return $ Jump j'
stripResult (JumpIf p j1 j2) = do
  j1' <- stripBlockJump j1
  j2' <- stripBlockJump j2
  return $ JumpIf p j1' j2'

stripBlockJump :: BlockJump -> Tree BlockJump
stripBlockJump (BlockJump target args) = do
  argOrder <- stripBlock target
  -- argOrder = [0, 2]
  -- args = [Int, Bool, Float, Long]
  -- newArgs = [Int, Float]
  let newArgs = argOrder <&> \arg -> args !! arg
  return $ BlockJump target newArgs

class Sub a where
  apply :: HashMap Id Id -> a -> a

instance Sub Statement where
  apply spec (Let name typ expr) = 
    Let name typ $ apply spec expr

instance Sub Expr where
  apply spec (Atom atom) = 
    Atom $ apply spec atom
  apply spec (Primitive p args) = 
    Primitive p $ (apply spec) `fmap` args
  apply spec (Call func args) = 
    Call func $ (apply spec) `fmap` args
  
instance Sub Atom where
  apply spec (Variable var) = Variable $ fromMaybe var $ HashMap.lookup var spec
  apply _ l@(Literal _) = l

instance Sub Result where
  apply spec (Return atom) = Return $ apply spec atom
  apply spec (Jump j) = Jump $ apply spec j
  apply spec (JumpIf p j1 j2) = JumpIf (apply spec p) (apply spec j1) (apply spec j2)

instance Sub BlockJump where
  apply spec (BlockJump target args) = BlockJump target $ args <&> apply spec
