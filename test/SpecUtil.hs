module SpecUtil
    ( loadLazyDataFile
    , loadStrictDataFile
    ) where

import Data.Semigroup ((<>))
import System.Environment (getEnv)

import qualified Data.ByteString as BS (ByteString, readFile)
import qualified Data.ByteString.Lazy as BL (ByteString, readFile)

loadLazyDataFile :: FilePath -> IO BL.ByteString
loadLazyDataFile = readTestFile BL.readFile

loadStrictDataFile :: FilePath -> IO BS.ByteString
loadStrictDataFile = readTestFile BS.readFile

readTestFile :: (FilePath -> IO a) -> FilePath -> IO a
readTestFile f path = do
    prefixPath <- getEnv "PROJECT_ROOT"
    f (prefixPath <> "/test/data/" <> path)
