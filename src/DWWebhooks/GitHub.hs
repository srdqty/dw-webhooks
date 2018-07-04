module DWWebhooks.GitHub where

import Data.Text (Text)

data PullRequestAction
    = Assigned
    | Unassigned
    | ReviewRequested
    | ReviewRequestRemoved
    | Labeled
    | Unlabeled
    | Opened
    | Edited
    | Closed
    | Reopened
    deriving (Eq, Ord, Show)

newtype BranchName = BranchName Text
    deriving (Eq, Ord, Show)

newtype CommitSha = CommitSha Text
    deriving (Eq, Ord, Show)

newtype RepoOwner = RepoOwner Text
    deriving (Eq, Ord, Show)

newtype RepoName = RepoName Text
    deriving (Eq, Ord, Show)

data PullRequest = PullRequest
    { action :: PullRequestAction
    , branchName :: BranchName
    , sha :: CommitSha
    , repoOwner :: RepoOwner
    , repoName :: RepoName
    } deriving (Eq, Ord, Show)
