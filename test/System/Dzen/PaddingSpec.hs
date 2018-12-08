{-# LANGUAGE OverloadedStrings #-}
module System.Dzen.PaddingSpec (spec) where

import Test.Hspec
import System.Dzen (DString, toString)
import System.Dzen.Padding

spec :: Spec
spec = do
  let txt = "foo" :: DString
  describe "padL" $ do
    specify "ex1" $ toString (padL 5 txt) `shouldBe` "^ib(1)  foo"
  describe "padR" $ do
    specify "ex1" $ toString (padR 5 txt) `shouldBe` "^ib(1)foo  "
  describe "padC" $ do
    specify "ex1" $ toString (padC 5 txt) `shouldBe` "^ib(1) foo "
    specify "ex2" $ toString (padC 6 txt) `shouldBe` "^ib(1)  foo "
    specify "ex3" $ toString (padC 7 txt) `shouldBe` "^ib(1)  foo  "
