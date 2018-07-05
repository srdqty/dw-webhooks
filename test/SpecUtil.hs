module SpecUtil where

import Data.Semigroup ((<>))
import System.Environment (getEnv)

import Data.ByteString (ByteString)
import qualified Data.ByteString as B (readFile)

loadDataFile :: FilePath -> IO ByteString
loadDataFile path = do
    prefixPath <- getEnv "PROJECT_ROOT"
    B.readFile (prefixPath <> "/test/data/" <> path)
