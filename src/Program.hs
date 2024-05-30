{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Program(
  Name,
  Program(..),
  Function(..),
  Expr(..),
  GenericArg(..),
  Argument(..),
  Type(..),
  Statement(..),
  ArgumentType(..),
  CallType(..),
  Literal(..),
  FunctionType(..),
  Primitive(..)
) where

type Name = String

data Program t = Program [Function t]

data Function t = Function Name [GenericArg] [Argument t] (Expr t)

data GenericArg = GenericArg Name deriving (Eq, Ord)
data Argument t = Argument Name (ArgumentType t)

data Literal = Number Integer deriving (Eq, Ord)

data Expr t 
  = Variable Name t 
  | Block [Statement t] (Maybe (Expr t)) 
  | Literal Literal
  | Call Name [Expr t] (CallType t)
  | Primitive Primitive [Expr t]
  | If (Expr t) (Expr t) (Expr t)
--deriving instance (Show (CallType t), Show t) => Show (Expr t)

data Primitive
  = Plus
  | Minus
  | Times
  | Div
  | Equals
  deriving (Eq, Ord)

data Statement t = Let Name t (Expr t)
--deriving instance (Show (CallType t), Show t) => Show (Statement t)

data Type = Named Name | Generic Name deriving (Eq, Ord)

data FunctionType = FunctionType [GenericArg] [Type] Type deriving (Show, Eq, Ord)

data family ArgumentType t
data instance ArgumentType () = AParse Type
data instance ArgumentType Type = AType Type

data family CallType t
data instance CallType () = CParse
data instance CallType Type = CType [Type] Type

instance Semigroup (Program t) where
  (Program a) <> (Program b) = Program $ a <> b

instance Monoid (Program t) where
  mempty = Program []

instance Show Primitive where
  show Plus = "+"
  show Minus = "-"
  show Times = "*"
  show Div = "/"
  show Equals = "=="

instance Show Literal where
  show (Number n) = show n

instance (Show (CallType t), Show t) => Show (Statement t) where
  show (Let name typ value) = "let " <> (show name) <> ": " <> (show typ) <> " = " <> (show value) <> "\n"

instance (Show (CallType t), Show t) => Show (Expr t) where
  show (Variable name typ) = name <> ":" <> (show typ)
  show (Block statements result) = "{\n" <> (mconcat $ show `fmap` statements) <> (maybe "" (((<>) "") . show) result) <> "}"
  show (Literal lit) = show lit
  show (Call func args call) = func <> (show call) <> "(" <> (show args) <> ")"
  show (Primitive p args) = (show p) <> "(" <> (show args) <> ")"
  show (If p e1 e2) = "if " <> (show p) <> " then " <> (show e1) <> " else " <> (show e2)

instance Show Type where
  show (Named name) = name
  show (Generic generic) = generic

instance Show (CallType ()) where
  show (CParse) = ""
instance Show (CallType Type) where
  show (CType generics result) = "{" <> (show generics) <> " | " <> (show result) <> "}"

instance Show (ArgumentType ()) where
  show (AParse t) = show t
instance Show (ArgumentType Type) where
  show (AType t) = show t

instance Show GenericArg where
  show (GenericArg name) = name

instance Show (ArgumentType t) => Show (Argument t) where
  show (Argument name t) = name <> ": " <> (show t)

instance (Show (ArgumentType t), Show (CallType t), Show t) => Show (Function t) where
  show (Function name generics args expr) = "func " <> name <> (show generics) <> (show args) <> " = " <> (show expr)

instance (Show (ArgumentType t), Show (CallType t), Show t) => Show (Program t) where
  show (Program functions) = "Program[" <> (mconcat $ ((<>) "\n" . show) `fmap` functions) <> "\n]"
