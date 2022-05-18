--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Char
import           Data.List
import           Data.Monoid      (mappend)
import           GHC.IO.Exception
import           Hakyll
import           Text.Pandoc
import           System.Process
import qualified Data.Text as T

--------------------------------------------------------------------------------
config :: Configuration
config = defaultConfiguration { destinationDirectory = "docs" }

main :: IO ()
main = hakyllWith config $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler


    match "files/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "contact.org" $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext'
            >>= relativizeUrls

    match "projects.org" $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext'
            >>= relativizeUrls

    match "blog/*" $ do
        route $ setExtension "html"
        compile $ mathJaxAddedCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    create ["blog.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "blog/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Blog"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= relativizeUrls


    match "index.org" $ do
        route $ setExtension "html"
        compile $ do
            pandocCompiler
                >>= loadAndApplyTemplate "templates/default.html" defaultContext'
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler

    create ["rss.xml"] $ do
      route idRoute
      compile $ do
        let feedCtx = postCtx `mappend` bodyField "description"
        posts <- fmap (take 10) . recentFirst =<<
          loadAllSnapshots "blog/*" "content"
        renderRss myFeedConfiguration feedCtx posts
          -- Remove (take 10) when there are enough posts

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" <>
    defaultContext'

defaultContext' :: Context String
defaultContext' =
  versionField "commit" Full <>
  defaultContext


    -- match (fromList ["about.rst", "contact.markdown"]) $ do
    --     route   $ setExtension "html"
    --     compile $ pandocCompiler
    --         >>= loadAndApplyTemplate "templates/default.html" defaultContext
    --         >>= relativizeUrls

--------------------------------------------------------------------------------
myFeedConfiguration :: FeedConfiguration
myFeedConfiguration = FeedConfiguration
    { feedTitle       = "James Leslie's Blog"
    , feedDescription = "This feed provides my blog posts!"
    , feedAuthorName  = "James Leslie"
    , feedAuthorEmail = "jamesleslie@posteo.net"
    , feedRoot        = "https://jeslie0.github.com"
    }
--------------------------------------------------------------------------------

-- * Userjy
-- This mathjax config was taken from
-- https://userjy.github.io/posts/2021-08-23-HakyllSetupMathjax.html
-- Many thanks!
mathjaxExtensions :: Extensions
mathjaxExtensions = extensionsFromList
                    [Ext_tex_math_dollars --  $...$ or $$...$$
                    ,Ext_tex_math_double_backslash --  \(...\) or \[...\]
                    ,Ext_latex_macros
                    ,Ext_inline_code_attributes
                    ]
--Step 2: Setup ReaderOptions using the Extensions from Step 1
readMathjaxOptions :: ReaderOptions
readMathjaxOptions = defaultHakyllReaderOptions
                {
                    readerExtensions = (readerExtensions defaultHakyllReaderOptions) <> mathjaxExtensions
                }
--Step 3: Setup WriterOptions
writeMathjaxOptions :: WriterOptions
writeMathjaxOptions = defaultHakyllWriterOptions
                {
                    writerHTMLMathMethod = MathJax ""
                }
--Step 4: Build the compiler using the ReaderOption and Writer Option from Step 2, 3.
mathJaxAddedCompiler :: Compiler (Item String)
mathJaxAddedCompiler = pandocCompilerWith readMathjaxOptions writeMathjaxOptions



-- * Ysndr
-- The following was taken from
-- https://blog.ysndr.de/posts/internals/2020-03-22-built-with-hakyll-part-2/
-- Many thanks!


-- Git related fields
--------------------------------------------------------------------------------
data GitVersionContent = Hash | Commit | Full
     deriving (Eq, Read)

instance Show GitVersionContent where
    show content = case content of
        Hash   -> "%h"
        Commit -> "%h: %s"
        Full   -> "%h: %s (%ai)"

-- Query information of a given file tracked with git
getGitVersion :: GitVersionContent -- Kind of information
              -> FilePath          -- File to query information of
              -> IO String         --
getGitVersion content path = do
    (status, stdout, _) <- readProcessWithExitCode "git" [
        "log",
        "-1",
        "--format=" ++ (show content),
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
    getGitVersion content  path

-- Field that contains the commit hash of HEAD.
headVersionField :: String -> GitVersionContent -> Context String
headVersionField name content  = field name $ \_ -> unsafeCompiler $ getGitVersion content  "."

readTimeField :: String -> Snapshot -> Context String
readTimeField name snapshot = field name $ \item -> do
    body <- itemBody <$> loadSnapshot (itemIdentifier item) snapshot
    let words = length (T.words . T.pack $ body)
    return $ show $ div words 200
