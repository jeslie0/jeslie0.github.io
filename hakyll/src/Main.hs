{-# LANGUAGE OverloadedStrings #-}

module Main where

import Compilers
import Contexts
import Feed
import GitCommit
import Hakyll (Configuration (provideMetadata), Identifier, MonadMetadata, PageNumber, bodyField, buildPaginateWith, compile, composeRoutes, compressCssCompiler, constField, copyFileCompiler, create, defaultConfiguration, defaultContext, fromFilePath, hakyllWith, idRoute, listField, loadAll, loadAllSnapshots, loadAndApplyTemplate, makeItem, match, paginateContext, paginateEvery, paginateRules, recentFirst, relativizeUrls, renderRss, route, saveSnapshot, setExtension, sortRecentFirst, templateBodyCompiler)
import Metadata
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

  pages <- buildPaginateWith grouper "site/woody/**.org" makeId

  paginateRules pages $ \pageNum patt -> do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll patt
      let paginateCtx = paginateContext pages pageNum
          ctx =
            listField "woodyPosts" woodyPostCtx (return posts)
              <> constField "title" ("Woody - Page " <> show pageNum)
              <> headVersionField "commit" HashAndDate
              <> paginateCtx
              <> defaultContext

      makeItem ""
        >>= loadAndApplyTemplate "templates/image-archive.html" ctx
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

-- Paginate
grouper :: (MonadMetadata m, MonadFail m) => [Identifier] -> m [[Identifier]]
grouper = fmap (paginateEvery 6) . sortRecentFirst

makeId :: PageNumber -> Identifier
makeId pageNum = fromFilePath $ "woody/" <> show pageNum <> "/index.html"
