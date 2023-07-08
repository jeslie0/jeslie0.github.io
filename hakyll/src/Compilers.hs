module Compilers where

import Hakyll.Core.Compiler
import Hakyll.Core.Item
import Hakyll.Web.Pandoc
import Text.Pandoc.Shared

shiftedHeaderPandocCompiler :: Compiler (Item String)
shiftedHeaderPandocCompiler =
  pandocCompilerWithTransform defaultHakyllReaderOptions defaultHakyllWriterOptions (headerShift 1)
