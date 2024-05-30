module Main (main) where

import Parser
import Typer
import Monomorph
import Lower
import Strip
import Text.Trifecta(parseString, Result(..), ErrInfo)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Control.Exception(throwIO, Exception)

main :: IO ()
main = repl

env :: Env
env = Env {
  variables=HashMap.empty,
  namedTypes=HashSet.fromList ["Int", "Bool"],
  generics=HashSet.empty,
  functions=HashMap.empty
}

instance Exception ErrInfo

repl :: IO ()
repl = do
  text <- getProgramText
  program <- case parseString parseProgram mempty text of
    Success p -> return p
    Failure e -> throwIO e

  typed <- case evalTyper (typeProgram program) env of
    Right p -> return p
    Left e -> throwIO e
  print typed

  let ((), _, morphed) = runMorpher $ morphProgram typed
  print morphed

  let lowered = lowerProgram morphed
  print lowered

  let stripped = stripProgram lowered
  print stripped

  repl

getProgramText :: IO String
getProgramText = do
  line <- getLine
  if line == ""
    then return line
    else do
      rest <- getProgramText
      return $ line <> "\n" <> rest