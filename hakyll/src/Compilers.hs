{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Compilers where

import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as C
import Data.Text.Encoding (decodeUtf8)
import GHC.IO.Exception
import Hakyll
import Hakyll.Core.Compiler
import Hakyll.Core.Item
import Hakyll.Web.Pandoc
import System.Directory
import System.Process
import Text.Pandoc.Definition
import Text.Pandoc.Options
import Text.Pandoc.Shared
import Text.Pandoc.Walk
import Network.URI.Encode
import Data.Time.Clock.System

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
latexTransform = walkM renderLatex

latexToSvg :: T.Text -> Compiler B.ByteString
latexToSvg code =
   unsafeCompiler $ do
    time <- getSystemTime
    let tmpDir = "/tmp/texfrag/"
        dviFile = tmpDir <> show time.systemNanoseconds <> ".dvi"
        svgFile = tmpDir <> show time.systemNanoseconds <> ".svg"
        texFile = tmpDir <> show time.systemNanoseconds <> ".tex"

    createDirectoryIfMissing True tmpDir

    TIO.writeFile texFile $ firstLatex <> code <> secondLatex

    readProcess "lualatex" ["--interaction=nonstopmode", "--shell-escape", "--output-format=dvi", "--output-directory=" <> tmpDir, texFile] ""

    readProcess "dvisvgm" [dviFile, "-n", "-b", "min", "-c", "1.5", "-o", svgFile] ""

    B.readFile svgFile


firstLatex = "\\documentclass{article}\n\\usepackage[pdftex,active,tightpage]{preview}\n\\usepackage{amsmath}\n\\usepackage{tikz}\n\\usepackage{tikz-cd}\n\\usepackage{amsthm}\n\\usepackage{mathrsfs}\n\\usepackage{physics}\n\\usetikzlibrary{matrix}\n\\usepackage{xcolor}\n\\definecolor{fg}{HTML}{839496}\n\n\\begin{document}\n\\begin{preview}\n"

secondLatex = "\\end{preview}\n\\end{document}\n"
