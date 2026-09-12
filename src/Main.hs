{-# LANGUAGE OverloadedStrings #-}

module Main where

import Compilers
import Contexts
import Feed
import GitCommit
import Hakyll (Configuration (provideMetadata), Identifier, MonadMetadata, PageNumber, Rules, bodyField, buildPaginateWith, buildTags, compile, composeRoutes, compressCssCompiler, constField, copyFileCompiler, create, defaultConfiguration, defaultContext, fromCapture, fromFilePath, hakyllWith, idRoute, listField, loadAll, loadAllSnapshots, loadAndApplyTemplate, makeItem, match, paginateContext, paginateEvery, paginateRules, recentFirst, relativizeUrls, renderRss, route, saveSnapshot, setExtension, sortRecentFirst, tagsRules, templateBodyCompiler)
import Metadata
import Misc (titleCase)
import Routes

configuration :: Configuration
configuration =
  defaultConfiguration {provideMetadata = pandocMetadata (Just "")}

main :: IO ()
main = hakyllWith configuration $ do
  staticRules
  blogRule
  indexRule
  woodyRule
  notesArchiveRule
  noteRule
  generalOrgRule
  rssRule

  match "templates/**" $
    compile templateBodyCompiler

staticRules :: Rules ()
staticRules = do
  match "site/images/**" $ do
    route stripSite
    compile copyFileCompiler

  match "site/files/**" $ do
    route stripSite
    compile copyFileCompiler

  match "site/style.css" $ do
    route stripSite
    compile compressCssCompiler

blogRule :: Rules ()
blogRule = do
  tags <- buildTags "site/blog/**.org" (fromCapture "tags/*.html")

  tagsRules tags $ \tagStr tagsPattern -> do
    route $ composeRoutes (fileToIndexDirWith "tags") (setExtension "html")
    compile $ do
      posts <- loadAll tagsPattern >>= recentFirst
      let postsCtx =
            constField "title" (titleCase tagStr)
              <> listField "posts" blogPostCtx (return posts)
              <> defaultContext'
      makeItem ""
        >>= loadAndApplyTemplate "templates/tag-page.html.in" postsCtx
        >>= relativizeUrls

  match "site/blog/**.org" $ do
    route $ composeRoutes stripSite (setExtension "html")
    compile $
      shiftedHeaderPandocCompiler
        >>= loadAndApplyTemplate "templates/post.html.in" blogPostCtx
        >>= saveSnapshot "content"
        >>= relativizeUrls
        >>= minifyHtmlCompiler

indexRule :: Rules ()
indexRule =
  create ["index.html"] $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "site/blog/**.org"
      let archiveCtx =
            listField "posts" blogPostCtx (return posts)
              <> constField "title" "Blog"
              <> headVersionField "commit" HashAndDate
              <> defaultContext
      makeItem ""
        >>= \ident ->
          loadAndApplyTemplate "templates/archive.html.in" archiveCtx ident
            >>= relativizeUrls
            >>= minifyHtmlCompiler

notesArchiveRule :: Rules ()
notesArchiveRule =
  create ["notes/index.html"] $ do
    route idRoute
    compile $ do
      notes <- loadAll "site/notes/**.org"
      let archiveCtx =
            listField "notes" blogPostCtx (return notes)
              <> constField "title" "Notes"
              <> headVersionField "commit" HashAndDate
              <> defaultContext
      makeItem ""
        >>= \ident ->
          loadAndApplyTemplate "templates/notes-archive.html.in" archiveCtx ident
            >>= relativizeUrls
            >>= minifyHtmlCompiler

noteRule :: Rules ()
noteRule =
  match "site/notes/**.org" $ do
    route $ composeRoutes fileToIndexDir (setExtension "html")
    compile $
      shiftedHeaderPandocCompiler
        >>= loadAndApplyTemplate "templates/note.html.in" defaultContext'
        >>= saveSnapshot "content"
        >>= relativizeUrls
        >>= minifyHtmlCompiler

rssRule :: Rules ()
rssRule = create ["rss.xml"] $ do
  route idRoute
  compile $ do
    let feedCtx = blogPostCtx `mappend` bodyField "description"
    posts <-
      fmap (take 10) . recentFirst
        =<< loadAllSnapshots "site/blog/**.org" "content"
    renderRss myFeedConfiguration feedCtx posts

woodyRule :: Rules ()
woodyRule = do
  match "site/woody/**.org" $ do
    route $ composeRoutes stripSite (setExtension "html")
    compile $
      shiftedHeaderPandocCompiler
        >>= saveSnapshot "woodyContent"
        >>= loadAndApplyTemplate "templates/image-post.html.in" woodyPostCtx
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
        >>= loadAndApplyTemplate "templates/image-archive.html.in" ctx
        >>= relativizeUrls
        >>= minifyHtmlCompiler

-- Remove (take 10) when there are enough posts

generalOrgRule :: Rules ()
generalOrgRule =
  match "site/*.org" $ do
    route $ composeRoutes fileToIndexDir (setExtension "html")
    compile $
      shiftedHeaderPandocCompiler
        >>= loadAndApplyTemplate "templates/default.html.in" defaultContext'
        >>= relativizeUrls
        >>= minifyHtmlCompiler

-- Paginate
grouper :: (MonadMetadata m, MonadFail m) => [Identifier] -> m [[Identifier]]
grouper = fmap (paginateEvery 6) . sortRecentFirst

makeId :: PageNumber -> Identifier
makeId pageNum = fromFilePath $ "woody/" <> show pageNum <> "/index.html"
