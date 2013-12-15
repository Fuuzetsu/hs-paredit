module HSParedit.DeleteSpec (main, spec) where

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
  describe "when checking sanity" $ do
    it "ensures fromTree . toTree identity" $ do
      fromTree (toTree zipTree') == zipTree'

  describe "paren insertion" $ do
    it "inserts parens at after code symbol if immediately after it" $ do
      let tinyTree :: Tree Code
          tinyTree = Node { rootLabel = TopLevel
                          , subForest = [sc "foo"]
                          }

          editedTree :: Tree Code
          editedTree = Node { rootLabel = TopLevel
                            , subForest = [ sc "foo"
                                          , Node RoundNode []
                                          ]
                            }
        in tinyTree `shouldBe` editedTree
