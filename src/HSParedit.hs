{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}

-- |
-- Module      :  HSParedit
-- Copyright   :  (c) Mateusz Kowalczyk 2013
-- License     :  GPL-3
--
-- Maintainer  :  fuuzetsu@fuuzetsu.co.uk
-- Stability   :  experimental
-- Portability :  portable

module HSParedit where

import Data.Functor
import Data.Monoid
import Data.Tree
import Data.Tree.Zipper
import Data.Tree.Pretty

data Code = CodeSymbol | RoundNode | SquareNode | StringNode
    | SymbolChar Char | TopLevel deriving Eq


instance Show Code where
    show CodeSymbol = "CodeSymbol"
    show RoundNode = "RoundNode"
    show SquareNode = "SquareNode"
    show (SymbolChar c) = [c]
    show StringNode = "StringNode"
    show TopLevel = "TopLevel"

onTree :: (TreePos Full a -> TreePos Full b) -> Tree a -> Tree b
onTree f = toTree . f . fromTree

sc :: String -> Tree Code
sc xs = Node CodeSymbol (map (\x -> Node (SymbolChar x) []) xs)

basicTree :: Tree Code
basicTree = Node { rootLabel = TopLevel
                 , subForest = [ sc "foo"
                               , sc "bar"
                               , Node { rootLabel = RoundNode
                                      , subForest = [ sc "qux"
                                                    , sc "quux"
                                                    ]
                                      }
                               ]
                 }

type EC = TreePos Empty Code
type FC = TreePos Full Code

zipTree :: TreePos Full Code
zipTree = fromTree basicTree

leftNodes :: TreePos Full a -> [Tree a]
leftNodes = gatherSiblings prev . prev

rightNodes :: TreePos Full a -> [Tree a]
rightNodes = gatherSiblings next . next

deleteRightNodes :: TreePos Full a -> TreePos Full a
deleteRightNodes = deleteNodes next prevTree

deleteLeftNodes :: TreePos Full a -> TreePos Full a
deleteLeftNodes = deleteNodes prev nextTree

-- | This function keeps deleting the neighbours picked with @f@ and @g@
-- one by one, refocusing on our original node at each deletion.
-- Terminates when there are no more neigbours to remove.
-- See 'deleteLeftNodes' and 'deleteRightNodes' for example usage.
deleteNodes :: (TreePos Full a -> Maybe (TreePos Full a))
               -- ^ Advance to next neighbour
            -> (TreePos Empty a -> Maybe (TreePos Full a))
               -- ^ Move back to original node after deletion
            -> TreePos Full a -> TreePos Full a
deleteNodes f g n = case f n of
    Nothing -> n -- no siblings left
    Just n' -> case g $ delete n' of
        Nothing -> n -- something went pretty damn wrong, return original
        Just n'' -> deleteNodes f g n''


gatherSiblings :: (TreePos Full a -> Maybe (TreePos Full a))
               -> Maybe (TreePos Full a) -> [Tree a]
gatherSiblings _ Nothing = []
gatherSiblings f (Just n) = tree n : gatherSiblings f (f n)

openGeneric :: Code -> Either EC FC -> TreePos Full Code
openGeneric c (Left t) = case parent t of
    Nothing -> insT c t -- We shouldn't be hitting no-parent case.
    Just p  -> insT c $ nextSpace p
openGeneric c (Right t) = case parent . delete $ deleteRightNodes t of
    Nothing -> insT c $ nextSpace t -- shouldn't happen
    Just p -> case label p of
      CodeSymbol -> insert (Node CodeSymbol siblings)
                    (nextSpace . insT c $ nextSpace p)
      _ -> t -- Only split CodeSymbols now

  where
    -- Rememeber our neighbours
    siblings :: [Tree Code]
    siblings = tree t : rightNodes t

    insParens :: Tree Code -> Tree Code
    insParens code = undefined

openRound :: Either EC FC -> TreePos Full Code
openRound = openGeneric RoundNode

openSquare :: Either EC FC -> TreePos Full Code
openSquare = openGeneric SquareNode

closeRound :: TreePos Full Code -> TreePos Empty Code
closeRound = nextSpace

insT :: Code -> TreePos Empty Code -> TreePos Full Code
insT t = insert (Node t [])

dr :: Tree Code -> IO ()
dr = putStrLn . drawVerticalTree . fmap show

dt :: TreePos Full Code -> IO ()
dt = putStrLn . drawVerticalTree . fmap show . toTree
