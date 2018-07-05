{-# LANGUAGE OverloadedStrings #-}

module DWWebhooks.GitHubSpec (main, spec) where

import Data.Aeson (eitherDecode, encode)

import Test.Hspec
    ( Spec
    , describe
    , hspec
    , it
    , shouldBe
    , xit
    )

import SpecUtil (loadDataFile)
import DWWebhooks.GitHub

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    pullRequestActionSpec
    branchNameSpec
    commitShaSpec
    repoOwnerSpec
    repoNameSpec
    pullRequestSpec

pullRequestActionSpec :: Spec
pullRequestActionSpec = undefined

branchNameSpec :: Spec
branchNameSpec = describe "BranchName" $ do
    describe "FromJSON" $
        it "should decode JSON successfully" $
            eitherDecode "\"name\"" `shouldBe` Right (BranchName "name")
    describe "ToJSON" $
        it "should encode JSON successfully" $
            encode (BranchName "name") `shouldBe` "\"name\""

commitShaSpec :: Spec
commitShaSpec = describe "CommitSha" $ do
    describe "FromJSON" $
        it "should decode JSON successfully" $
            eitherDecode "\"sha\"" `shouldBe` Right (CommitSha "sha")
    describe "ToJSON" $
        it "should encode JSON successfully" $
            encode (CommitSha "sha") `shouldBe` "\"sha\""

repoOwnerSpec :: Spec
repoOwnerSpec = describe "RepoOwner" $ do
    describe "FromJSON" $
        it "should decode JSON successfully" $
            eitherDecode "\"owner\"" `shouldBe` Right (RepoOwner "owner")
    describe "ToJSON" $
        it "should encode JSON successfully" $
            encode (RepoOwner "owner") `shouldBe` "\"owner\""

repoNameSpec :: Spec
repoNameSpec = describe "RepoName" $ do
    describe "FromJSON" $
        it "should decode JSON successfully" $
            eitherDecode "\"repo\"" `shouldBe` Right (RepoName "repo")
    describe "ToJSON" $
        it "should encode JSON successfully" $
            encode (RepoName "repo") `shouldBe` "\"repo\""

pullRequestSpec :: Spec
pullRequestSpec = describe "PullRequest" $ do
    describe "FromJSON" $
        xit "should parse expected JSON input" $ do
            jsonData <- loadDataFile "GitHub/pullrequestevent.json"
            True `shouldBe` False
    describe "ToJSON" $
        xit "whatever" $ True `shouldBe` True
