{-# LANGUAGE OverloadedStrings #-}
module Contexts where

import Hakyll
import Hakyll.Web.Template.Context
import GitCommit

blogPostCtx :: Context String
blogPostCtx =
    dateField "date" "%B %e, %Y" <>
    defaultContext'


defaultContext' :: Context String
defaultContext' =
  versionField "commit" HashAndDate <>
  defaultContext
