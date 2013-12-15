{-# LANGUAGE CPP #-}

module HSParedit.DeleteSpec (main, spec) where

import HSParedit
import Data.Tree
import Data.Tree.Zipper
import Test.Hspec

main :: IO ()
main = hspec spec

basicTree' :: Tree String
basicTree' = Node { rootLabel = "program"
                 , subForest = [ Node { rootLabel = "foo"
                                      , subForest = [] }
                               , Node { rootLabel = "bar"
                                      , subForest = [] }
                               , Node { rootLabel = "baz"
                                      , subForest = [ Node { rootLabel = "qux"
                                                           ,  subForest = [] }
                                                    ]
                                      }
                               ]
                 }

zipTree' :: TreePos Full String
zipTree' = root $ fromTree basicTree

spec :: Spec
spec = do
  describe "when deleting forward" $ do
    it "ensures test sanity" $ do
      1 == 1

