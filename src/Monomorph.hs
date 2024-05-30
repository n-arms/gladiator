module Monomorph (
  morphProgram,
  runMorpher
) where

import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Writer.Strict
import Control.Monad.Trans.Class
import Typed
import Typer(apply)
import Program(Name, Type(..), CallType(..), ArgumentType(..), Literal(..), Primitive(..), GenericArg(..))
import Data.HashMap.Strict(HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe(catMaybes)
import Data.Functor
import Control.Applicative
import Control.Monad

type Morpher a = StateT Env (Writer Program) a

data Unmorphed = Unmorphed Function Integer

data Env = Env {
  unmorphed :: HashMap Name Unmorphed,
  generics :: HashMap Name Type
}

runMorpher :: Morpher a -> (a, Env, Program)
runMorpher morpher = let
  ((a, state), program) = runWriter $ runStateT morpher $ Env { unmorphed=HashMap.empty, generics=HashMap.empty}
  in (a, state, program)

defineFunction :: Function -> Morpher ()
defineFunction func@(Function name _ _ _) = do
  env <- get
  put $ env { unmorphed = HashMap.insert name (Unmorphed func 0) $ unmorphed env }

generateFunction :: Name -> [Argument] -> Expr -> Morpher Name
generateFunction name arguments body = do
  env <- get
  let Unmorphed func counter = (unmorphed env) HashMap.! name
  put $ env { unmorphed = HashMap.insert name (Unmorphed func $ counter + 1) $ unmorphed env }
  let newName = name <> "_" <> (show counter)
  let func = Function newName [] arguments body
  lift $ tell $ Program [func]
  return newName

defineGeneric :: Name -> Type -> Morpher ()
defineGeneric name typ = do
  env <- get
  put $ env { generics = HashMap.insert name typ $ generics env }

defineGenerics :: HashMap Name Type -> Morpher ()
defineGenerics generics = 
    HashMap.foldlWithKey' 
      (\state name typ -> do
        state
        defineGeneric name typ
        return ())
      (return ())
      generics

lookupGeneric :: Name -> Morpher Type
lookupGeneric generic = do
  env <- get
  return $ (generics env) HashMap.! generic

lookupFunction :: Name -> Morpher Function
lookupFunction name = do
  env <- get
  let Unmorphed func _ = (unmorphed env) HashMap.! name
  return $ func

morphProgram :: Program -> Morpher ()
morphProgram (Program functions) = do
  functions <- sequence $ functions <&> visitFunction
  lift $ tell $ Program $ catMaybes functions
  return ()

-- if the function has no generics, then morph it and return it
-- if the function has generics, morph its contents but don't return it
visitFunction :: Function -> Morpher (Maybe Function)
visitFunction func@(Function name generics arguments body) = do
  defineFunction func
  if length generics == 0
    then do
      body <- morphExpr body
      return $ Just $ Function name generics arguments body
    else 
      return Nothing

morphFunction :: Function -> [Type] -> Morpher Name
morphFunction func@(Function name generics args body) generic_args = do
  let genericNames = generics <&> \(GenericArg name) -> name
  let spec = HashMap.fromList $ zip genericNames generic_args
  defineGenerics spec
  body <- morphExpr body
  if (length generics) == 0
    then return name
    else do
      let arguments = args <&> \(Argument n (AType t)) -> Argument n $ AType $ apply spec t
      generateFunction name arguments body

morphExpr :: Expr -> Morpher Expr
morphExpr (Variable name typ) = do
  typ <- morphType typ
  return $ Variable name typ
morphExpr (Block statements result) = do 
  statements <- sequence $ statements <&> morphStatement
  result <- maybe (return Nothing) (\result -> Just `fmap` (morphExpr result)) result
  return $ Block statements result
morphExpr (Literal (Number number)) = return $ Literal $ Number number
morphExpr (Call function arguments (CType generics result)) = do
  arguments <- sequence $ arguments <&> morphExpr
  oldFunc <- lookupFunction function
  generics <- sequence $ generics <&> morphType
  funcName <- morphFunction oldFunc $ generics
  result <- morphType result
  return $ Call funcName arguments $ CType [] result
morphExpr (Primitive p arguments) = do
  arguments <- sequence $ arguments <&> morphExpr
  return $ Primitive p arguments
morphExpr (If p e1 e2) = do
  p <- morphExpr p
  e1 <- morphExpr e1
  e2 <- morphExpr e2
  return $ If p e1 e2

morphStatement :: Statement -> Morpher Statement
morphStatement (Let name typ expr) = do
  typ <- morphType typ
  expr <- morphExpr expr
  return $ Let name typ expr
  

morphType :: Type -> Morpher Type
morphType (Named name) = return $ Named name
morphType (Generic generic) = do
  env <- get
  return $ case HashMap.lookup generic $ generics env of
    Just typ -> typ
    Nothing -> error $ "Unknown generic" <> generic 
