{-# LANGUAGE OverloadedStrings #-}

module Contexts where

import GitCommit
import Hakyll
import Hakyll.Web.Template.Context

blogPostCtx :: Context String
blogPostCtx =
  dateField "date" "%B %e, %Y"
    <> defaultContext'

woodyPostCtx :: Context String
woodyPostCtx =
  dateField "date" "%B %e, %Y"
    <> teaserField "tease" "woodyContent"
    <> defaultContext'

defaultContext' :: Context String
defaultContext' =
  versionField "commit" HashAndDate
    <> defaultContext
