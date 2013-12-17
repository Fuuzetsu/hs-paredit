-- |
-- Module      :  HSParedit.Types
-- Copyright   :  (c) Mateusz Kowalczyk 2013
-- License     :  GPL-3
--
-- Maintainer  :  fuuzetsu@fuuzetsu.co.uk
-- Stability   :  experimental
-- Portability :  portable
module HSParedit.Types where

data Code = CodeSymbol | RoundNode | SquareNode | StringNode
    | SymbolChar Char | TopLevel deriving Eq


instance Show Code where
    show CodeSymbol = "CodeSymbol"
    show RoundNode = "RoundNode"
    show SquareNode = "SquareNode"
    show (SymbolChar c) = [c]
    show StringNode = "StringNode"
    show TopLevel = "TopLevel"
