{-# LANGUAGE OverloadedStrings #-}
module GitCommit where

import Hakyll.Web.Template.Context
import Hakyll.Core.Compiler
import Hakyll.Core.Identifier
import Hakyll.Core.Item
import GHC.IO.Exception
import qualified Data.Text as T
import Data.List
import Data.Char
import System.Process

data GitVersionContent = Hash | Commit | Full | HashAndDate
     deriving (Eq, Read)

instance Show GitVersionContent where
    show content = case content of
        Hash   -> "%h"
        Commit -> "%h: %s"
        Full   -> "%h: %s (%ai)"
        HashAndDate -> "%h on %as, by %an"

-- Query information of a given file tracked with git
getGitVersion :: GitVersionContent -- Kind of information
              -> FilePath          -- File to query information of
              -> IO String         --
getGitVersion content path = do
    (status, stdout, _) <- readProcessWithExitCode "git" [
        "log",
        "-1",
        "--format=" ++ show content,
        "--",
        "./" ++ path] ""

    return $ case status  of
        ExitSuccess -> trim stdout
        _           -> ""

    where trim = dropWhileEnd isSpace

-- Field that contains the latest commit hash that hash touched the current item.
versionField :: String -> GitVersionContent -> Context String
versionField name content = field name $ \item -> unsafeCompiler $ do
    let path = toFilePath $ itemIdentifier item
    getGitVersion content path

-- Field that contains the commit hash of HEAD.
headVersionField :: String -> GitVersionContent -> Context String
headVersionField name content  = field name $ \_ -> unsafeCompiler $ getGitVersion content  "."

