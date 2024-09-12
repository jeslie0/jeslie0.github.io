{-# LANGUAGE OverloadedStrings #-}

module GitCommit where

import Data.Char ( isSpace )
import Data.List ( dropWhileEnd )
import GHC.IO.Exception ( ExitCode(ExitSuccess) )
import Hakyll.Core.Compiler ( unsafeCompiler )
import Hakyll.Core.Identifier ( toFilePath )
import Hakyll.Core.Item ( Item(itemIdentifier) )
import Hakyll.Web.Template.Context ( field, Context )
import System.Process ( readProcessWithExitCode )

data GitVersionContent
  = -- | Just the hash
    Hash
  | -- | Hash and commit message
    Commit
  | -- | Hash, commit message and time
    Full
  | -- | Hash, date and author
    HashAndDate
  deriving (Eq, Read)

instance Show GitVersionContent where
  show content = case content of
    Hash -> "%h"
    Commit -> "%h: %s"
    Full -> "%h: %s (%ai)"
    HashAndDate -> "%h on %as, by %an"

-- Query information of a given file tracked with git
getGitVersion ::
  GitVersionContent -> -- Kind of information
  FilePath -> -- File to query information of
  IO String --
getGitVersion content path = do
  (status, stdout, _) <-
    readProcessWithExitCode
      "git"
      [ "log",
        "-1",
        "--format=" ++ show content,
        "--",
        "./" ++ path
      ]
      ""

  return $ case status of
    ExitSuccess -> trim stdout
    _ -> ""
  where
    trim = dropWhileEnd isSpace

-- | Field that contains the latest commit hash that hash touched the current item.
versionField :: String -> GitVersionContent -> Context String
versionField name content = field name $ \item -> unsafeCompiler $ do
  let path = toFilePath $ itemIdentifier item
  getGitVersion content path

-- | Field that contains the commit hash of HEAD.
headVersionField :: String -> GitVersionContent -> Context String
headVersionField name content = field name $ \_ -> unsafeCompiler $ getGitVersion content "."
