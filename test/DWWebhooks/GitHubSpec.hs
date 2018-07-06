{-# LANGUAGE OverloadedStrings #-}

module DWWebhooks.GitHubSpec (main, spec) where

import Data.Aeson (eitherDecode, encode)

import Test.Hspec
    ( Spec
    , describe
    , hspec
    , it
    , shouldBe
    )

import SpecUtil (loadLazyDataFile)
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
    pullRequestEventSpec

pullRequestActionSpec :: Spec
pullRequestActionSpec = describe "PullRequestAction" $ do
    describe "FromJSON" $
        it "should decode JSON successfully" $ do
            eitherDecode "\"assigned\"" `shouldBe` Right Assigned
            eitherDecode "\"unassigned\"" `shouldBe` Right Unassigned
            eitherDecode "\"review_requested\"" `shouldBe` Right ReviewRequested
            eitherDecode "\"review_request_removed\"" `shouldBe` Right ReviewRequestRemoved
            eitherDecode "\"labeled\"" `shouldBe` Right Labeled
            eitherDecode "\"unlabeled\"" `shouldBe` Right Unlabeled
            eitherDecode "\"opened\"" `shouldBe` Right Opened
            eitherDecode "\"edited\"" `shouldBe` Right Edited
            eitherDecode "\"closed\"" `shouldBe` Right Closed
            eitherDecode "\"reopened\"" `shouldBe` Right Reopened
    describe "ToJSON" $
        it "should encode JSON successfully" $ do
            encode Assigned `shouldBe` "\"assigned\""
            encode Unassigned `shouldBe` "\"unassigned\""
            encode ReviewRequested `shouldBe` "\"review_requested\""
            encode ReviewRequestRemoved `shouldBe` "\"review_request_removed\""
            encode Labeled `shouldBe` "\"labeled\""
            encode Unlabeled `shouldBe` "\"unlabeled\""
            encode Opened `shouldBe` "\"opened\""
            encode Edited `shouldBe` "\"edited\""
            encode Closed `shouldBe` "\"closed\""
            encode Reopened `shouldBe` "\"reopened\""

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

pullRequestEventSpec :: Spec
pullRequestEventSpec = describe "PullRequestEvent" $
    describe "FromJSON" $
        it "should decode JSON successfully" $ do
            jsonData <- loadLazyDataFile "GitHub/pullrequestevent.json"
            eitherDecode jsonData `shouldBe` Right event
    where
        event = PullRequestEvent
            Closed
            (BranchName "changes")
            (CommitSha "34c5c7793cb3b279e22454cb6750c80560547b3a")
            (RepoOwner "Codertocat")
            (RepoName "Hello-World")
