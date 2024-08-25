module ColourFilter where

import Data.Vector qualified as V

-- The purpose of this module is to create a function that converts
-- hexcode colours to a css filter. This will make it easier to always
-- have tikzcd diagrams being the correct colour.

data Colour = Colour
  { r :: {-# UNPACK #-} !Float,
    g :: {-# UNPACK #-} !Float,
    b :: {-# UNPACK #-} !Float
  } deriving (Eq, Show)


multiply :: Colour -> V.Vector Float -> Colour
multiply (Colour {r, g, b}) arr =
  Colour newR newG newB
  where
    newR =
      r * (arr V.! 0) + g * (arr V.! 1) + b * (arr V.! 2)

    newG =
      r * (arr V.! 3) + g * (arr V.! 4) + b * (arr V.! 5)

    newB =
      r * (arr V.! 6) + g * (arr V.! 7) + b * (arr V.! 8)
