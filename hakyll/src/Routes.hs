module Routes where

import Hakyll.Core.Routes
import Hakyll.Core.Identifier

stripSite :: Routes
stripSite =
  customRoute $ \id -> drop (length "site/") $ toFilePath id
