module Parser(
  parseProgram,
) where

import Text.Trifecta
import qualified Text.Parser.Token.Highlight as Highlight
import qualified Data.HashSet as HashSet
import Data.Maybe(fromMaybe)
import Control.Applicative
import Parsed
import Precedence
import Program(Type(..), GenericArg(..), ArgumentType(..), Literal(..), CallType(..), Primitive(..))

variableStyle = IdentifierStyle {
  _styleName="Variable",
  _styleStart=lower,
  _styleLetter=alphaNum,
  _styleHighlight=Highlight.Identifier,
  _styleReservedHighlight=Highlight.ReservedIdentifier,
  _styleReserved=HashSet.fromList [("if"), ("func"), ("let")]
}

typeNameStyle = IdentifierStyle {
  _styleName="Type Name",
  _styleStart=upper,
  _styleLetter=alphaNum,
  _styleHighlight=Highlight.Identifier,
  _styleReservedHighlight=Highlight.ReservedIdentifier,
  _styleReserved=HashSet.fromList [("if"), ("func"), ("let")]
}

variable :: Parser String
variable = ident variableStyle

typeName :: Parser String
typeName = ident typeNameStyle

parseLiteral :: Parser Literal
parseLiteral = Number <$> integer

parseProgram :: Parser Program
parseProgram = do
  functions <- many parseFunction
  return $ Program functions

parseFunction :: Parser Function
parseFunction = do
  symbol "func"
  name <- variable
  maybeGenerics <- optional $ brackets $ commaSep parseGeneric
  arguments <- parens $ commaSep parseArgument
  symbol "="
  expr <- parseExpr
  let generics = fromMaybe [] maybeGenerics
  return $ Function name generics arguments expr

parseCall :: Parser Expr
parseCall = do
  function <- try $ do
    function <- variable
    symbol "("
    return function
  args <- commaSep parseExpr
  symbol ")"
  return $ Call function args CParse


parseExpr :: Parser Expr
parseExpr = operators [
    \expr -> do
      left <- expr
      rest <- many $ do
        char <- (symbol "*") <|> (symbol "/")
        right <- expr
        let primitive = if char == "*" then Times else Div
        return (primitive, right)
      return $ foldl (\left (primitive, right) -> Primitive primitive [left, right]) left rest,
    \expr -> do
      left <- expr
      rest <- many $ do
        char <- (symbol "+") <|> (symbol "-")
        right <- expr
        let primitive = if char == "+" then Plus else Minus
        return (primitive, right)
      return $ foldl (\left (primitive, right) -> Primitive primitive [left, right]) left rest,
    \expr -> do
      left <- expr
      rest <- many $ do
        symbol "=="
        expr
      return $ foldl (\a b -> Primitive Equals [a, b]) left rest
  ] parseNonOpExpr

parseNonOpExpr :: Parser Expr
parseNonOpExpr = parseCall <|> parseVariable <|> parseBlock <|> (Literal <$> parseLiteral) <|> parseIf <|> parseParens

parseParens :: Parser Expr
parseParens = do
  symbol "("
  expr <- parseExpr
  symbol ")"
  return expr

parseIf :: Parser Expr
parseIf = do
  symbol "if"
  predicate <- parseExpr
  symbol "then"
  consequent <- parseExpr
  symbol "else"
  alternative <- parseExpr
  return $ If predicate consequent alternative

parseBlock :: Parser Expr
parseBlock = braces $ do
  statements <- many $ do
    statement <- parseStatement
    symbol ";"
    return statement
  result <- optional parseExpr
  return $ Block statements result

parseStatement :: Parser Statement
parseStatement = do
  symbol "let"
  name <- variable
  symbol "="
  expr <- parseExpr
  return $ Let name () expr

parseVariable :: Parser Expr
parseVariable = (flip Variable ()) <$> variable 

parseArgument :: Parser Argument
parseArgument = do
  var <- variable
  symbol ":"
  typ <- parseType
  return $ Argument var $ AParse typ

parseGeneric :: Parser GenericArg
parseGeneric = GenericArg <$> variable

parseType :: Parser Type
parseType = (Named <$> typeName) <|> (Generic <$> variable)