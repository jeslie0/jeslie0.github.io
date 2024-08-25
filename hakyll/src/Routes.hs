module Routes where

import Hakyll.Core.Routes
import Hakyll.Core.Identifier

stripSite :: Routes
stripSite =
  customRoute $ \iden -> drop (length "site/") $ toFilePath iden
