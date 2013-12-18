-- |
-- Module      :  HSParedit.Printer
-- Copyright   :  (c) Mateusz Kowalczyk 2013
-- License     :  GPL-3
--
-- Maintainer  :  fuuzetsu@fuuzetsu.co.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- This module provides some convenience printing functions, such as
-- printing the AST into a LISP/Scheme format.
module HSParedit.Printer where

import Data.List (intersperse)
import Data.Tree
import HSParedit.Types

toScheme :: Tree Code -> String
toScheme (Node CodeSymbol x2) = concat $ map toScheme x2
toScheme (Node RoundNode x2) = wrap "(" ")" . unwords $ map toScheme x2
toScheme (Node SquareNode x2) = wrap "[" "]" . unwords $ map toScheme x2
toScheme (Node StringNode x2) = wrap "\"" "\"" . unwords $ map toScheme x2
toScheme (Node (SymbolChar x1) _) = [x1]
toScheme (Node TopLevel x2) = concat . intersperse "\n\n" $ map toScheme x2


wrap :: String -> String -> String -> String
wrap o e c = o ++ c ++ e
