{-# LANGUAGE OverloadedStrings #-}
module Main where

import Hakyll.Main

import Hakyll.Core.Compiler
import Hakyll.Core.Rules
import Hakyll.Core.Routes
import Hakyll.Core.File
import Hakyll.Core.Item
import Hakyll.Web.CompressCss
import Hakyll.Web.Template
import Hakyll.Web.Template.Context
import Hakyll.Web.Template.List
import Hakyll.Web.Pandoc
import Hakyll.Web.Html.RelativizeUrls

import GitCommit
import Compilers
import Contexts
import Routes

main :: IO ()
main = hakyll $ do

  match "site/images/*" $ do
    route stripSite
    compile copyFileCompiler

  match "site/files/*" $ do
    route stripSite
    compile copyFileCompiler

  match "site/css/*" $ do
    route   stripSite
    compile compressCssCompiler

  match "site/blog/*.org" $ do
    route $ composeRoutes stripSite (setExtension "html")
    compile $ shiftedHeaderPandocCompiler
        >>= loadAndApplyTemplate "templates/post.html" blogPostCtx
        >>= saveSnapshot "content"
        >>= loadAndApplyTemplate "templates/default.html" defaultContext'
        >>= relativizeUrls

  create ["blog.html"] $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "site/blog/*"
      let archiveCtx =
            listField "posts" blogPostCtx (return posts) <>
            constField "title" "Blog" <>
            headVersionField "commit" HashAndDate <>
            defaultContext

      makeItem ""
        >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
        >>= relativizeUrls

  match "site/*.org" $ do
    route $ composeRoutes stripSite (setExtension "html")
    compile $ shiftedHeaderPandocCompiler
      >>= loadAndApplyTemplate "templates/default.html" defaultContext'
      >>= relativizeUrls

  match "templates/*" $
    compile templateBodyCompiler
