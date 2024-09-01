module Routes where

import Hakyll.Core.Identifier
import Hakyll.Core.Routes
import System.FilePath

-- | Remove the initial site prefix
stripSite :: Routes
stripSite =
  customRoute $ joinPath . tail . splitPath . toFilePath

-- | Remove the initial site prefix
fileToIndexDir :: Routes
fileToIndexDir =
  customRoute $ \ident -> (takeBaseName . toFilePath $ ident) </> "index.html"
