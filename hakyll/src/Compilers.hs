{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Compilers where

import Control.Monad
import Control.Exception
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as C
import Data.Maybe
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Data.Text.IO qualified as TIO
import Data.Time.Clock.System
import GHC.IO.Exception
import Hakyll
import Hakyll.Core.Compiler
import Hakyll.Core.Item
import Hakyll.Web.Pandoc
import Network.URI.Encode
import System.Directory
import System.Process
import System.Environment
import Text.Pandoc.Definition
import Text.Pandoc.Options
import Text.Pandoc.Shared
import Text.Pandoc.Walk

mathExtensions :: Extensions
mathExtensions =
  extensionsFromList
    [ Ext_tex_math_single_backslash, -- TeX math btw (..) [..]
      Ext_tex_math_double_backslash, -- TeX math btw \(..\) \[..\]
      Ext_tex_math_dollars, -- TeX math between $..$ or $$..$$
      Ext_latex_macros, -- Parse LaTeX macro definitions (for math only)
      Ext_inline_code_attributes, -- Ext_inline_code_attributes
      Ext_abbreviations, -- PHP markdown extra abbreviation definitions
      Ext_raw_tex
    ]

pandocWriterOptions :: WriterOptions
pandocWriterOptions =
  defaultHakyllWriterOptions
    { writerHTMLMathMethod = MathJax ""
    }

pandocReaderOptions :: ReaderOptions
pandocReaderOptions =
  defaultHakyllReaderOptions
    { readerExtensions = defaultHakyllReaderOptions.readerExtensions <> mathExtensions
    }

shiftedHeaderPandocCompiler :: Compiler (Item String)
shiftedHeaderPandocCompiler =
  pandocCompilerWithTransformM pandocReaderOptions pandocWriterOptions $ latexTransform . headerShift 1

minifyHtmlCompiler :: Item String -> Compiler (Item String)
minifyHtmlCompiler item = do
  let minifyCmd = "minify" :: String
  minified <-
    unsafeCompiler $
      readProcessWithExitCode minifyCmd ["--type", "html"] item.itemBody

  case minified of
    (ExitSuccess, minifiedOutput, _) -> return $ itemSetBody minifiedOutput item
    (_, _, _) -> return item

renderLatex :: Block -> Compiler Block
renderLatex (RawBlock (Format "latex") code) = do
  svgContent <- latexToSvg code
  let imgSrc = "data:image/svg+xml;utf8," <> (encodeText . decodeUtf8 $ svgContent)
  return $ Para [Image ("", ["latexfragment"], []) [] (imgSrc, "")]
renderLatex block = return block

latexTransform :: Pandoc -> Compiler Pandoc
latexTransform = walkM latexCompiler

latexToSvg :: T.Text -> Compiler B.ByteString
latexToSvg code = do
  unsafeCompiler $ do
    time <- getSystemTime
    mTmpDir <- lookupEnv "TMPDIR"
    let tmpDir = fromMaybe "/tmp" mTmpDir
        texfragDir = tmpDir <> "/texfrag/"
        dviFile = texfragDir <> show time.systemNanoseconds <> ".dvi"
        svgFile = texfragDir <> show time.systemNanoseconds <> ".svg"
        texFile = texfragDir <> show time.systemNanoseconds <> ".tex"

    createDirectoryIfMissing True texfragDir

    TIO.writeFile texFile code

    (luaExitCode, luaStdOut, luaStdErr) <- readProcessWithExitCode "lualatex" ["--interaction=nonstopmode", "--shell-escape", "--output-format=dvi", "--output-directory=" <> texfragDir, texFile] ""
    if luaExitCode == ExitSuccess then return () else print luaStdOut >> print "\n\n" >> print luaStdErr

    (dviExitCode, dviStdOut, dviStdErr) <- readProcessWithExitCode "dvisvgm" [dviFile, "-n", "-b", "min", "-c", "1.5", "-o", svgFile] ""
    if dviExitCode == ExitSuccess then return () else print dviStdOut >> print "\n\n" >> print dviStdErr

    B.readFile svgFile


latexContext :: Context T.Text
latexContext =
  mconcat
    [ field "body" (\item -> return . T.unpack $ item.itemBody),
      field "latex_header" $ \item -> do
        metadata <- getMetadata item.itemIdentifier
        return $ fromMaybe "" $ lookupString "latex_header" metadata
    ]

latexCompiler :: Block -> Compiler Block
latexCompiler (RawBlock (Format "latex") code) =
  (imageBlock . ("data:image/svg+xml;utf8," <>) . (encodeText . decodeUtf8) . itemBody <$>) $
    makeItem code
      >>= loadAndApplyTemplate "templates/latex.tex" latexContext
      >>= withItemBody (return . T.pack >=> latexToSvg)
  where
    imageBlock imgSrc = Para [Image ("", ["latexfragment"], []) [] (imgSrc, "")]
latexCompiler block = return block


printPossibleError :: Exception e => Either e a -> IO ()
printPossibleError (Left err) = print err
printPossibleError _ = return ()
