module Precedence(
  operators
) where

import Control.Applicative
import Text.Trifecta

operators :: [Parser a -> Parser a] -> Parser a -> Parser a
operators table base = 
  foldl combine base table
  where
    combine state operator = (try $ operator state) <|> state
