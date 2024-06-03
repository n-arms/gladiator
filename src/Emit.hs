module Emit (
  
) where

import Virtual
import VCode(VReg(..), Arg(..), Res(..), Fun(..))
import qualified IR
import IR(Id)
import Program(Type(..))
import qualified Program
import Control.Monad.Trans.State.Strict
import Data.Functor
import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict(HashMap)
{-
data Env = Env {
  next_reg :: Id,
  identifiers :: HashMap Id VReg
}

type Emit a = State Env a

unusedReg :: Emit VReg
unusedReg = do
  env <- get
  let reg = next_reg env
  put $ env {
    next_reg = reg + 1
  }
  return $ VReg reg

defineId :: Id -> VReg -> Emit ()
defineId id reg = do
  env <- get
  put $ env {
    identifiers = HashMap.insert id reg $ identifiers env
  }

lookupId :: Id -> Emit VReg
lookupId id = do
  env <- get
  return $ (identifiers env) HashMap.! id

emitFunction :: IR.Function -> Function
emitFunction (IR.Function name blocks) = do
  let (Block args _ _) = head blocks
  newArgs <- sequence $ args <&> \_ -> unusedReg
  undefined
  

emitBlock :: IR.Block -> Emit Block
emitBlock (IR.Block args stmts result) = do
  newArgs <- sequence $ args <&> \(IR.Argument typ) -> do {
    size <- typeSize typ;
    sequence $ replicate size $ unusedReg <&> ResV
  }
  newStmts <- sequence $ stmts <&> emitStatement
  newResult <- emitResult result
  return $ Block newArgs $ (mconcat newStmts) <> newResult

emitStatement :: IR.Statement -> Emit [Instr]
emitStatement (IR.Let name typ expr) = do
  reg <- unusedReg
  defineId name reg
  emitExpr reg typ expr

emitExpr :: VReg -> Type -> IR.Expr -> Emit [Instr]
emitExpr dest _ (IR.Atom (IR.Variable id)) = do
  src <- lookupId id
  return [Move (ResV dest) (ArgV src)]
emitExpr dest _ (IR.Atom (IR.Literal literal)) = return [
    MoveAbs (ResV dest) literal
  ]
emitExpr dest _ (IR.Primitive primitive args) = do
  loadToDest <- case args of
    [IR.Variable var, _] -> do {
      src <- lookupId var;
      return $ Move (ResV dest) (ArgV src)
    }
    [IR.Literal literal, _] ->
      return $ MoveAbs (ResV dest) literal
  (srcReg, loadSrc) <- case args of
    [_, IR.Variable var] -> do {
      src <- lookupId var;
      return (src, [])
    }
    [_, IR.Literal literal] -> do {
      src <- unusedReg;
      return (src, [MoveAbs (ResV src) literal])
    }
  let instr = case primitive of
        Program.Plus -> FAdd (ResV dest) $ ArgV srcReg
        Program.Minus -> FSub (ResV dest) $ ArgV srcReg
  return $ [loadToDest] <> loadSrc <> [instr]
emitExpr dest _ (IR.Call func args) = do
  argRegs <- sequence $ replicate (length args) unusedReg
  loadToArgs <- sequence $ zipUsing args argRegs $ \arg dest ->
    case arg of
      IR.Variable var -> do {
        reg <- lookupId var;
        return $ Move (ResV dest) $ ArgV reg
      }
      IR.Literal literal -> return $ MoveAbs (ResV dest) literal
  result <- unusedReg
  let instr = Call (FunV func) (ArgV result) $ argRegs <&> ArgV
  let loadResult = Move (ResV dest) (ArgV result)
  return $ loadToArgs <> [instr] <> [loadResult]

zipUsing :: [a] -> [b] -> (a -> b -> c) -> [c]
zipUsing a b f = zipWith f a b
-- return the block size in **number of 64 bit blocks**
typeSize :: Type -> Emit Int
typeSize (Named name) = if name == "Bool" || name == "Int" then return 1 else error $ "Unsupported type " <> name

emitResult :: IR.Result -> Emit [Instr]
emitResult (IR.Return atom) = do
  src <- unusedReg
  loadToSrc <- case atom of
    IR.Variable var -> do {
      dest <- lookupId var;
      return $ Move (ResV src) $ ArgV dest
    }
    IR.Literal literal -> do {
      return $ MoveAbs (ResV src) literal
    }
  let instr = Return $ ArgV src
  return [loadToSrc, instr]
-}