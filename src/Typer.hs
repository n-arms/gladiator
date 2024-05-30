module Typer (
  typeProgram,
  Env(..),
  evalTyper,
  apply
) where

import Typed
import qualified Parsed
import Program(Type(..), Name, GenericArg(..), ArgumentType(..), CallType(..), FunctionType(..), Literal(..), Primitive(..))
import Data.HashMap.Strict(HashMap)
import Data.HashSet(HashSet)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Class
import Data.Functor
import Control.Monad
import Data.Maybe(fromMaybe)
import Control.Exception(Exception)

data Error 
  = UnknownVariable Name 
  | UndefinedNamedType Name
  | UnknownFunction Name
  | TypeMismatch Type Type
  | UnspecializedGeneric Name
  | OverlyspecializedArgument Name Name
  | IncorrectNumberOfPrimitiveArguments Primitive [Expr]
  deriving Show
instance Exception Error

data Env = Env {
  variables :: HashMap Name Type,
  namedTypes :: HashSet Name,
  generics :: HashSet Name,
  functions :: HashMap Name FunctionType
}

type Typer a = StateT Env (Either Error) a
evalTyper :: Typer a -> Env -> Either Error a
evalTyper = evalStateT 

report :: Error -> Typer a
report = lift . Left

getVariable :: Name -> Typer Type
getVariable variable = do
  env <- get
  let typ = HashMap.lookup variable $ variables env
  lift $ maybe (Left $ UnknownVariable variable) Right typ

getNamedType :: Name -> Typer ()
getNamedType namedType = do
  env <- get
  let defined = HashSet.member namedType $ namedTypes env
  if defined
    then return ()
    else report $ UndefinedNamedType namedType
    
getGeneric :: Name -> Typer ()
getGeneric generic = do
  env <- get
  let defined = HashSet.member generic $ generics env
  if defined
    then return ()
    else report $ UndefinedNamedType generic

getFunction :: Name -> Typer FunctionType
getFunction function = do
  env <- get
  let typ = HashMap.lookup function $ functions env
  lift $ maybe (Left $ UnknownFunction function) Right typ

setVariable :: Name -> Type -> Typer ()
setVariable variable typ = do
  env <- get
  put $ env { variables=HashMap.insert variable typ $ variables env }

defineNamedType :: Name -> Typer ()
defineNamedType namedType = do
  env <- get
  put $ env { namedTypes=HashSet.insert namedType $ namedTypes env }

defineGeneric :: Name -> Typer ()
defineGeneric generic = do
  env <- get
  put $ env { generics=HashSet.insert generic $ generics env }

setFunction :: Name -> FunctionType -> Typer ()
setFunction function typ = do
  env <- get
  put $ env { functions=HashMap.insert function typ $ functions env }

locally :: Typer a -> Typer a
locally typer = do
  env <- get
  result <- typer
  put env
  return result

typeProgram :: Parsed.Program -> Typer Program
typeProgram (Program functions) = do
  functions <- sequence $ functions <&> typeFunction
  return $ Program functions

typeFunction :: Parsed.Function -> Typer Function
typeFunction (Function name generics arguments body) = do
  func <- locally $ do
    sequence $ generics <&> \(GenericArg name) -> defineGeneric name
    arguments <- sequence $ arguments <&> typeArgument
    body <- typeExpr body
    return $ Function name generics arguments body
  let (Function _ _ _ body) = func
  setFunction name $ FunctionType generics (arguments <&> \(Argument _ (AParse typ)) -> typ) $ getType body
  return func

typeArgument :: Parsed.Argument -> Typer Argument
typeArgument (Argument name (AParse typ)) = do
  setVariable name typ
  return $ Argument name $ AType typ

typeExpr :: Parsed.Expr -> Typer Expr
typeExpr (Variable name _) = do
  typ <- getVariable name
  return $ Variable name typ
typeExpr (Block statements result) = locally $ do
  statements <- sequence $ statements <&> typeStatement
  result <- maybe (return Nothing) (\expr -> (typeExpr expr) <&> Just) result
  return $ Block statements result
typeExpr (Literal (Number number)) = return $ Literal $ Number number
typeExpr (Call function arguments _) = do
  arguments <- sequence $ arguments <&> typeExpr
  funcType <- getFunction function
  typ <- specializeFunction funcType $ arguments <&> getType
  return $ Call function arguments typ
typeExpr (Primitive primitive arguments) = do
  arguments <- sequence $ arguments <&> typeExpr
  checkPrimitive primitive arguments
  return $ Primitive primitive arguments
typeExpr (If p e1 e2) = do
  p <- typeExpr p
  when ((getType p) /= boolType) $ report $ TypeMismatch (getType p) boolType
  e1 <- typeExpr e1
  e2 <- typeExpr e2
  let t1 = getType e1
  let t2 = getType e2
  when (t1 /= t2) $ report $ TypeMismatch t1 t2
  return $ If p e1 e2

checkPrimitive :: Primitive -> [Expr] -> Typer ()
checkPrimitive _ [left, right] = if getType left /= intType
  then report $ TypeMismatch intType $ getType left
  else if getType right /= intType
    then report $ TypeMismatch intType $ getType right
    else return ()
checkPrimitive primitive args = report $ IncorrectNumberOfPrimitiveArguments primitive args

specializeFunction :: FunctionType -> [Type] -> Typer (CallType Type)
specializeFunction (FunctionType generics arguments result) applicants = do
  specialized <- sequence $ zipWith specialize arguments applicants
  specializations <- foldM combineSpecializations HashMap.empty specialized
  generic_args <- sequence $ generics <&> \(GenericArg name) -> lift $ getSpecialization name specializations
  let specResult = apply specializations result
  return $ CType generic_args specResult
  where
    getSpecialization name spec = fromMaybe (Left $ UnspecializedGeneric name) ((HashMap.lookup name spec) <&> Right)

combineSpecializations :: HashMap Name Type -> HashMap Name Type -> Typer (HashMap Name Type)
combineSpecializations first second = do
  sequence $ HashMap.elems $ HashMap.intersectionWith (\a b -> if a /= b then report $ TypeMismatch a b else return ()) first second
  return $ first `HashMap.union` second

specialize :: Type -> Type -> Typer (HashMap Name Type)
specialize (Generic generic) specific = return $ HashMap.singleton generic specific
specialize (Named generic) (Generic specific) = report $ OverlyspecializedArgument specific generic
specialize general@(Named generalName) specific@(Named specificName) = 
  if generalName == specificName
    then return HashMap.empty
    else report $ TypeMismatch general specific

apply :: HashMap Name Type -> Type -> Type
apply spec (Named name) = Named name
apply spec (Generic generic) = fromMaybe (Generic generic) $ HashMap.lookup generic spec

typeStatement :: Parsed.Statement -> Typer Statement
typeStatement (Let name _ expr) = do
  expr <- typeExpr expr
  setVariable name $ getType expr
  return $ Let name (getType expr) expr
