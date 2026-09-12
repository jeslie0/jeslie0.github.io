{-# LANGUAGE OverloadedStrings #-}

module Contexts where

import GitCommit
import Hakyll
import Hakyll.Web.Template.Context

postTagsField :: Context String
postTagsField = do
  listFieldWith "tags" (field "tag" (pure . itemBody)) $ \postItem ->
    mapM makeItem =<< getTags (itemIdentifier postItem)

blogPostCtx :: Context String
blogPostCtx =
  dateField "date" "%B %e, %Y"
    <> postTagsField
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
