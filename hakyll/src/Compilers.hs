{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Compilers where

import Control.Monad
import Crypto.Hash as Hash
import Data.ByteString qualified as B
import Data.Maybe
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Text.IO qualified as T
import GHC.IO.Exception
import Hakyll
import Network.URI.Encode
import System.Directory
import System.Process
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

-- renderLatex :: Block -> Compiler Block
-- renderLatex (RawBlock (Format "latex") code) = do
--   svgContent <- latexToSvg code
--   let imgSrc = "data:image/svg+xml;utf8," <> (encodeText . decodeUtf8 $ svgContent)
--   return $ Para [Image ("", ["latexfragment"], []) [] (imgSrc, "")]
-- renderLatex block = return block

latexTransform :: Pandoc -> Compiler Pandoc
latexTransform = walkM latexCompiler

latexToSvg :: T.Text -> Compiler B.ByteString
latexToSvg latexFile = do
  unsafeCompiler $ do
    let tmpDir = "/tmp/texfrag/"
        hashCtx = Hash.hash @Hash.MD5 . encodeUtf8 $ latexFile
        texFile = tmpDir <> show hashCtx <> ".tex"
        dviFile = tmpDir <> show hashCtx <> ".dvi"
        svgFile = tmpDir <> show hashCtx <> ".svg"

    svgFileExists <- doesFileExist svgFile

    createDirectoryIfMissing True tmpDir

    unless svgFileExists $ do
      T.writeFile texFile latexFile

      void $ readProcess "lualatex" ["--interaction=nonstopmode", "--shell-escape", "--output-format=dvi", "--output-directory=" <> tmpDir, texFile] ""

      svgStr <- readProcess "dvisvgm" [dviFile, "-n", "-b", "min", "-c", "1.5", "-O", "none", "--relative", "-v", "0", "--stdout", "--currentcolor"] ""

      let idFix = T.replace "id='" ("id='" <> (T.pack . show $ hashCtx)) (T.pack svgStr)
          xlinkFix = T.replace "xlink:href='#" ("xlink:href='#" <> (T.pack . show $ hashCtx)) idFix

      T.writeFile svgFile xlinkFix

      removeFile texFile
      removeFile dviFile

    B.readFile svgFile

latexContext :: Context T.Text
latexContext =
  mconcat
    [ field "body" $ return . T.unpack . itemBody,
      field "latex_header" $ \item -> do
        metadata <- getMetadata item.itemIdentifier
        return $ fromMaybe "" $ lookupString "latex_header" metadata
    ]

latexCompiler :: Block -> Compiler Block
latexCompiler (RawBlock (Format "latex") code) =
  (makeContainer . decodeUtf8 . itemBody <$>) $ do
    item <- makeItem code
    latexContent <- loadAndApplyTemplate "templates/latex.tex" latexContext item
    withItemBody (return . T.pack >=> latexToSvg) latexContent
  where
    makeContainer content = Div ("", ["latexfragment"],[]) [RawBlock (Format "html") content]
latexCompiler block = return block
