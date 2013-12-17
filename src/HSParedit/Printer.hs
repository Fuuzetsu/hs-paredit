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

import Data.Tree
import HSParedit.Types

toScheme :: Tree Code -> String
toScheme = const "unimplemented"
