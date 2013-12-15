module HSParedit.PareditSpec (main, spec) where

import Prelude hiding (last)
import HSParedit
import Data.Tree
import Data.Tree.Zipper
import Data.Tree.Pretty
import Test.Hspec (hspec, describe, it, Spec)
import Test.Hspec.Expectations.Pretty
import System.Console.Terminfo.PrettyPrint
import Text.PrettyPrint.Free

instance Show a => Pretty (Tree a) where
  pretty = text . ("\n" ++) . drawVerticalTree . fmap show
  prettyList = list . map pretty

instance Show a => PrettyTerm (Tree a)

main :: IO ()
main = hspec spec

basicTree' :: Tree Code
basicTree' = Node { rootLabel = TopLevel
                  , subForest = [ sc "foo"
                                , sc "bar"
                                , Node { rootLabel = RoundNode
                                       , subForest = [ sc "qux"
                                                     , sc "quux"
                                                     ]
                                       }
                                ]
                  }

tinyTree :: Tree Code
tinyTree = Node { rootLabel = TopLevel
                , subForest = [sc "foo"]
                }

zipTree' :: TreePos Full Code
zipTree' = fromTree basicTree

spec :: Spec
spec = do
  let x `tShouldBe` y = toTree x `shouldBe` y
  describe "when checking sanity" $ do
    it "ensures fromTree . toTree identity" $ do
      fromTree (toTree zipTree') == zipTree'

  describe "paren insertion" $ do
    it "inserts round parens at after code symbol if immediately after it" $ do
      openRound tinyFocus `tShouldBe` editedTree RoundNode

    it "inserts square parens at after code symbol if immediately after it" $ do
      openSquare tinyFocus `tShouldBe` editedTree SquareNode

    it "splits code symbol when inserting round parens" $ do
      openRound codeFocus `tShouldBe` editedCodeTree RoundNode

    it "splits code symbol when inserting square parens" $ do
      openSquare codeFocus `tShouldBe` editedCodeTree SquareNode

    -- it "inserts char at code symbol if immediately after it" $ do
    --   toTree (insT (SymbolChar 'z') tinyFocus) `shouldBe` editedTree'



codeFocus = Right $ case firstChild (fromTree tinyTree') >>= lastChild of
  Just x -> x
  _ -> undefined

tinyTree' :: Tree Code
tinyTree' = Node { rootLabel = TopLevel
                 , subForest = [sc "foo"]
                 }

editedTree :: Code -> Tree Code
editedTree c = Node { rootLabel = TopLevel
                    , subForest = [ sc "foo"
                                  , Node c []
                                  ]
                    }

editedCodeTree :: Code -> Tree Code
editedCodeTree c = Node { rootLabel = TopLevel
                        , subForest = [ sc "fo"
                                      , Node c []
                                      , sc "o"
                                      ]
                        }

tinyFocus' t =
  case firstChild t of
    Just x -> last $ children x
    _ -> undefined

tinyFocus = Left $ tinyFocus' $ fromTree tinyTree'
