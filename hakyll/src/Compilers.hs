{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Compilers where

import Hakyll.Core.Compiler
import Hakyll.Core.Item
import Hakyll.Web.Pandoc
import Text.Pandoc.Definition
import Text.Pandoc.Options
import Text.Pandoc.Shared
import Text.Pandoc.Walk
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory
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
  pandocCompilerWithTransformM pandocReaderOptions pandocWriterOptions (tikzcdTransformM . headerShift 1)


minifyHtmlCompiler :: Item String -> Compiler (Item String)
minifyHtmlCompiler item = do
  let minifyCmd = "minify" :: String
  minified <- unsafeCompiler $
    readProcessWithExitCode minifyCmd ["--type", "html"] item.itemBody

  case minified of
    (ExitSuccess, minifiedOutput, _) -> return $ itemSetBody minifiedOutput item
    (_, _, _) -> return item


--


tikzcdTransformM :: Pandoc -> Compiler Pandoc
tikzcdTransformM = walkM convertTikzCodeM

convertTikzCodeM :: Block -> Compiler Block
convertTikzCodeM (RawBlock (Format "latex") code) =
    if isTikzcdEnvironment code
        then do
            svgFilePath <- unsafeCompiler $ generateSvgFromTikzCode code
            return $ case svgFilePath of
                Just path -> Para [Image nullAttr [Str ""] (path, "")]
                Nothing -> RawBlock (Format "latex") code
        else return $ RawBlock (Format "latex") code
convertTikzCodeM block = return block

isTikzcdEnvironment :: T.Text -> Bool
isTikzcdEnvironment code =
  "\\begin{tikzcd}" `T.isPrefixOf` code && "\\end{tikzcd}" `T.isSuffixOf` code

generateSvgFromTikzCode :: T.Text -> IO (Maybe T.Text)
generateSvgFromTikzCode tikzCode = do
    let svgFilePath = "path/to/save/generated/svg.svg"
    let tikz2svgCmd = "tikz2svg --outfile " <> svgFilePath

    TIO.writeFile "tikzcd.tex" tikzCode
    _ <- system $ "pdflatex tikzcd.tex && " <> tikz2svgCmd
    fileExists <- doesFileExist svgFilePath
    if fileExists
        then return (Just . T.pack $ svgFilePath)
        else return Nothing
