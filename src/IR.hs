module IR (
  Program(..),
  Function(..),
  Argument(..),
  Block(..),
  BlockJump(..),
  Result(..),
  Statement(..),
  Expr(..),
  Atom(..),
  Id
) where

import Program(Name, Literal(..), Primitive(..), Type(..))
import Data.Functor

data Program = Program [Function]
data Function = Function Name [Block]
data Argument = Argument Type
type Id = Int
data Block = Block [Argument] [Statement] Result
data BlockJump = BlockJump Id [Atom]
data Result = Return Atom | Jump BlockJump | JumpIf Atom BlockJump BlockJump
data Statement = Let Id Type Expr
data Expr = Atom Atom | Primitive Primitive [Atom] | Call Name [Atom]
data Atom = Variable Id | Literal Literal

instance Show Atom where
  show (Variable i) = "$" <> (show i)
  show (Literal l) = show l

instance Show Expr where
  show (Atom a) = show a
  show (Primitive p args) = (show p) <> (show args)
  show (Call func args) = func <> (show args)

instance Show Statement where
  show (Let id typ expr) = "let $" <> (show id) <> ": " <> (show typ) <> " = " <> (show expr)

instance Show Result where
  show (Return atom) = "return " <> (show atom)
  show (Jump j) = "jump " <> (show j)
  show (JumpIf p j1 j2) = "jumpIf " <> (show p) <> " then " <> (show j1) <> " else " <> (show j2)

instance Show BlockJump where
  show (BlockJump id args) = "#" <> (show id) <> (show args)

instance Show Block where
  show (Block args statements result) = "\t" <> (show args) <> ":\n" <> (mconcat $ statements <&> \s -> "\t\t" <> (show s) <> "\n") <> "\t\t" <> (show result) <> "\n"

instance Show Argument where
  show (Argument typ) = show typ

instance Show Function where
  show (Function name blocks) = "func " <> name <> ":\n" <> (mconcat $ blocks <&> show)

instance Show Program where
  show (Program funcs) = mconcat $ funcs <&> show
