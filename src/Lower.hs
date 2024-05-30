{-# LANGUAGE ScopedTypeVariables #-}

module Lower (
  lowerProgram,
  lowerBodys,
  index
) where

import IR
import qualified Typed
import Program(Type(..), Literal(..), Primitive(..), Name, CallType(..), ArgumentType(..))
import qualified Program
import Control.Monad.Trans.State.Strict
import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict(HashMap)
import Data.Functor
import Debug.Trace
import Data.Maybe(fromJust)
import Data.List(foldl')
import Debug.Trace

type Builder a = State Env a

data Env = Env {
  blocks :: HashMap Id Block,
  current :: [Statement],
  funcVars :: [Type],
  nextBlock :: Id,
  variables :: HashMap Name Id,
  currentBlock :: Id,
  currentArguments :: [Argument]
}

runBuilder :: Builder a -> Env -> (a, Env)
runBuilder builder env = (runState builder) env

addStmt :: Statement -> Builder ()
addStmt stmt = do
  env <- get
  put $ env { current = (current env) <> [stmt] }

unusedId :: Type -> Builder Id
unusedId typ = do
  env <- get
  let id = length $ funcVars env
  put $ env { funcVars = (funcVars env) <> [typ] }
  return id

reserveBlock :: Builder Id
reserveBlock = do
  env <- get
  let id = nextBlock env
  put $ env { nextBlock = id + 1 }
  return id

setIds :: [Type] -> Builder ()
setIds ids = do
  env <- get
  put $ env { funcVars = ids }

addExpr :: Type -> Expr -> Builder Id
addExpr typ expr = do
  id <- unusedId typ
  let stmt = Let id typ expr
  addStmt stmt
  return id

defineVariable :: Name -> Id -> Builder ()
defineVariable name id = do
  env <- get
  let vars = variables env
  put $ env { variables = HashMap.insert name id vars }

defineVariables :: HashMap Name Id -> Builder ()
defineVariables vars = HashMap.foldlWithKey' (\state name id -> state >>= \_ -> defineVariable name id) (return ()) vars

lookupVariable :: Name -> Builder Id
lookupVariable name = do
  env <- get
  return $ (variables env) HashMap.! name

nextBlockArgs :: Builder ([Atom], [Argument])
nextBlockArgs = do
  env <- get
  let vars = funcVars env
  let args = [0..(length vars) - 1] <&> Variable
  let signature = vars <&> Argument
  return $ (args, signature)

finishBlock :: Result -> Builder ()
finishBlock result = do
  env <- get
  let arguments = currentArguments env
  let block = Block arguments (current env) result
  let id = currentBlock env
  put $ env { 
    blocks = HashMap.insert id block $ blocks env,
    current = []
  }

locally :: Id -> [Argument] -> Builder a -> Builder a
locally block args builder = do
  oldEnv <- get
  let funcVars = args <&> \(Argument t) -> t
  put $ oldEnv { current = [], currentBlock = block, currentArguments = args, funcVars = funcVars }
  result <- builder
  newEnv <- get
  put $ oldEnv { blocks = blocks newEnv }
  return result

setArguments :: [Argument] -> Builder ()
setArguments args = do
  env <- get
  put $ env { currentArguments = args }

setVars :: [Type] -> Builder ()
setVars vars = do
  env <- get
  put $ env { funcVars = vars }

getBlock :: Builder Id
getBlock = do
  env <- get
  return $ currentBlock env

setBlock :: Id -> Builder ()
setBlock block = do
  env <- get
  put $ env { currentBlock = block }

index :: [a] -> [(a, Int)]
index list = zip list [0..(length list) - 1]

lowerProgram :: Typed.Program -> Program
lowerProgram (Typed.Program funcs) = Program $ funcs <&> lowerFunction

lowerBodys :: Typed.Program -> [HashMap Id Block]
lowerBodys (Typed.Program funcs) = funcs <&> lowerBody

lowerBody :: Typed.Function -> HashMap Id Block
lowerBody func@(Typed.Function name _ args body) = 
  let builder = lowerFunctionBody func in
  let argTypes = args <&> \(Typed.Argument _ (AType typ)) -> typ in
  let env = Env {
    blocks = HashMap.empty,
    current = [],
    funcVars = argTypes,
    nextBlock = 1,
    variables = HashMap.fromList $ index $ args <&> \(Typed.Argument name _) -> name,
    currentBlock = 0,
    currentArguments = argTypes <&> Argument
  } in
  let ((), env') = runBuilder builder env in
  blocks env'

lowerFunction :: Typed.Function -> Function
lowerFunction func@(Typed.Function name _ args body) = 
  let builder = lowerFunctionBody func in
  let argTypes = args <&> \(Typed.Argument _ (AType typ)) -> typ in
  let env = Env {
    blocks = HashMap.empty,
    current = [],
    funcVars = argTypes,
    nextBlock = 1,
    variables = HashMap.fromList $ index $ args <&> \(Typed.Argument name _) -> name,
    currentBlock = 0,
    currentArguments = argTypes <&> Argument
  } in
  let ((), env') = runBuilder builder env in
  let numBlocks = length $ blocks env' in
  let loweredBlocks = [0..numBlocks-1] <&> \block -> (blocks env') HashMap.! block in
  Function name loweredBlocks

lowerFunctionBody :: Typed.Function -> Builder ()
lowerFunctionBody (Typed.Function name _ args body) = do
  result <- lowerExpr body
  finishBlock $ Return result

lowerExpr :: Typed.Expr -> Builder Atom
lowerExpr (Typed.Variable name typ) = do
  id <- lookupVariable name
  return $ Variable id
lowerExpr (Typed.Block stmts final) = do
  lowerStatements stmts
  lowerExpr $ fromJust final
lowerExpr (Typed.Literal l) = return $ Literal l
lowerExpr (Typed.Call func args (CType _ typ)) = do
  arguments <- sequence $ args <&> lowerExpr
  id <- addExpr typ $ Call func arguments
  return $ Variable id
lowerExpr p'@(Typed.Primitive p args) = do
  arguments <- sequence $ args <&> lowerExpr
  id <- addExpr (Typed.getType p') $ Primitive p arguments
  return $ Variable id
lowerExpr (Typed.If p e1 e2) = do
  p <- lowerExpr p
  block <- getBlock
  e1Block <- reserveBlock
  e2Block <- reserveBlock
  (args, blockArgs) <- nextBlockArgs
  returnBlock <- reserveBlock
  finishBlock (JumpIf p (BlockJump e1Block args) (BlockJump e2Block args)) 
  let eType = Typed.getType e1
  locally e1Block blockArgs $ do
    e <- lowerExpr e1
    returnVar <- unusedId eType
    finishBlock $ Jump $ BlockJump returnBlock $ ([0..returnVar-1] <&> Variable) <> [e]
  locally e2Block blockArgs $ do
    e <- lowerExpr e2
    returnVar <- unusedId eType
    finishBlock $ Jump $ BlockJump returnBlock $ ([0..returnVar-1] <&> Variable) <> [e]
  setBlock returnBlock
  setArguments $ blockArgs <> [Argument eType]
  setVars $ (blockArgs <&> \(Argument typ) -> typ) <> [eType]
  return $ Variable $ length blockArgs

lowerStatements :: [Typed.Statement] -> Builder ()
lowerStatements = foldl' (\state stmt -> state >>= \_ -> lowerStatement stmt) $ return ()

lowerStatement :: Typed.Statement -> Builder ()
lowerStatement (Typed.Let name typ expr) = do
  atom <- lowerExpr expr
  case atom of
    Variable var -> do
      defineVariable name var
    _ -> do
      id <- addExpr typ $ Atom atom
      defineVariable name id
      