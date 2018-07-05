module DWWebhooks.GitHubSpec (main, spec) where

import Test.Hspec
    ( Spec
    , hspec
    , describe
    , it
    , shouldBe
    )

import DWWebhooks.GitHub

main :: IO ()
main = hspec spec

spec :: Spec
spec = jsonParsingSpec

jsonParsingSpec :: Spec
jsonParsingSpec = describe "Parses JSON " $
    it "todo" $
        True `shouldBe` True
