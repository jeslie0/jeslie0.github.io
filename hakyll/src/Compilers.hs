{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Compilers where

import Hakyll.Core.Compiler
import Hakyll.Core.Item
import Hakyll.Web.Pandoc
import Text.Pandoc.Options
import Text.Pandoc.Shared
import System.Process
import GHC.IO.Exception

mathExtensions :: Extensions
mathExtensions =
  extensionsFromList
    [ Ext_tex_math_single_backslash, -- TeX math btw (..) [..]
      Ext_tex_math_double_backslash, -- TeX math btw \(..\) \[..\]
      Ext_tex_math_dollars, -- TeX math between $..$ or $$..$$
      Ext_latex_macros, -- Parse LaTeX macro definitions (for math only)
      Ext_inline_code_attributes, -- Ext_inline_code_attributes
      Ext_abbreviations -- PHP markdown extra abbreviation definitions
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
  pandocCompilerWithTransform pandocReaderOptions pandocWriterOptions (headerShift 1)


minifyHtmlCompiler :: Item String -> Compiler (Item String)
minifyHtmlCompiler item = do
  let minifyCmd = "minify" :: String
  minified <- unsafeCompiler $
    readProcessWithExitCode minifyCmd ["--type", "html"] item.itemBody

  case minified of
    (ExitSuccess, minifiedOutput, _) -> return $ itemSetBody minifiedOutput item
    (_, _, _) -> return item
