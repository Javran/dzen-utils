module Main where

import Test.Hspec

main :: IO ()
main = hspec $
  describe "dummy" $ specify "dummy expr" $ True `shouldBe` True
