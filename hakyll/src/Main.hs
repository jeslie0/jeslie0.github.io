{-# LANGUAGE OverloadedStrings #-}

module Main where

import Compilers
import Contexts
import Feed
import GitCommit
import Hakyll.Core.Compiler
import Hakyll.Core.Configuration
import Hakyll.Core.File
import Hakyll.Core.Routes
import Hakyll.Core.Rules
import Hakyll.Main
import Hakyll.Web.CompressCss
import Hakyll.Web.Feed
import Hakyll.Web.Html.RelativizeUrls
import Hakyll.Web.Template
import Hakyll.Web.Template.Context
import Hakyll.Web.Template.List
import MetaData
import Routes

configuration :: Configuration
configuration =
  defaultConfiguration {provideMetadata = pandocMetadata (Just "")}

main :: IO ()
main = hakyllWith configuration $ do
  match "site/images/**" $ do
    route stripSite
    compile copyFileCompiler

  match "site/files/**" $ do
    route stripSite
    compile copyFileCompiler

  match "site/css/**" $ do
    route stripSite
    compile compressCssCompiler

  match "site/blog/**.org" $ do
    route $ composeRoutes stripSite (setExtension "html")
    compile $
      shiftedHeaderPandocCompiler
        >>= loadAndApplyTemplate "templates/post.html" blogPostCtx
        >>= saveSnapshot "content"
        >>= loadAndApplyTemplate "templates/default.html" defaultContext'
        >>= relativizeUrls
        >>= minifyHtmlCompiler

  create ["blog/index.html"] $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "site/blog/**.org"
      let archiveCtx =
            listField "posts" blogPostCtx (return posts)
              <> constField "title" "Blog"
              <> headVersionField "commit" HashAndDate
              <> defaultContext

      makeItem ""
        >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
        >>= relativizeUrls
        >>= minifyHtmlCompiler

  match "site/woody/**.org" $ do
    route $ composeRoutes stripSite (setExtension "html")
    compile $
      shiftedHeaderPandocCompiler
        >>= saveSnapshot "woodyContent"
        >>= loadAndApplyTemplate "templates/image_post.html" woodyPostCtx
        >>= relativizeUrls
        >>= minifyHtmlCompiler

  create ["woody/index.html"] $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAllSnapshots "site/woody/**.org" "woodyContent"
      let archiveCtx =
            listField "woodyPosts" woodyPostCtx (return posts)
              <> constField "title" "Woody"
              <> headVersionField "commit" HashAndDate
              <> defaultContext

      makeItem ""
        >>= loadAndApplyTemplate "templates/image-archive.html" archiveCtx
        >>= relativizeUrls
        >>= minifyHtmlCompiler

  match "site/index.org" $ do
    route $ composeRoutes stripSite (setExtension "html")
    compile $
      shiftedHeaderPandocCompiler
        >>= loadAndApplyTemplate "templates/default.html" defaultContext'
        >>= relativizeUrls
        >>= minifyHtmlCompiler

  match "site/*.org" $ do
    route $ composeRoutes fileToIndexDir (setExtension "html")
    compile $
      shiftedHeaderPandocCompiler
        >>= loadAndApplyTemplate "templates/default.html" defaultContext'
        >>= relativizeUrls
        >>= minifyHtmlCompiler

  match "templates/**" $
    compile templateBodyCompiler

  create ["rss.xml"] $ do
    route idRoute
    compile $ do
      let feedCtx = blogPostCtx `mappend` bodyField "description"
      posts <-
        fmap (take 10) . recentFirst
          =<< loadAllSnapshots "site/blog/**.org" "content"
      renderRss myFeedConfiguration feedCtx posts

-- Remove (take 10) when there are enough posts
