module Misc where

import Data.Char (toLower, toUpper)

titleCase :: String -> String
titleCase "" = ""
titleCase (x : xs) = (toUpper x) : (toLower <$> xs)
