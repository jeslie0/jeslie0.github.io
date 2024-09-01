{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module ColourFilter where

import Data.Char (digitToInt)
import Data.Vector qualified as V
import Text.Printf (printf)

-- The purpose of this module is to create a function that converts
-- hexcode colours to a css filter. This will make it easier to always
-- have tikzcd diagrams being the correct colour.

data Colour = Colour
  { r :: {-# UNPACK #-} !Float,
    g :: {-# UNPACK #-} !Float,
    b :: {-# UNPACK #-} !Float
  }
  deriving (Eq, Show)

-- Clamp a value between 0 and 255
clamp :: (Num a, Ord a) => a -> a
clamp x = max 0 (min 255 x)

-- Helper function to set RGB values (equivalent to Colour#set in JS)
set :: Float -> Float -> Float -> Colour -> Colour
set r' g' b' _ = Colour (clamp r') (clamp g') (clamp b')

-- Convert hex string to RGB tuple
hexToRgb :: String -> Maybe (Float, Float, Float)
hexToRgb hex = do
  let shorthand =
        if length hex == 4
          then concatMap (\x -> [x, x]) (tail hex)
          else tail hex
  case shorthand of
    [r1, r2, g1, g2, b1, b2] ->
      Just
        ( fromIntegral (hexPair [r1, r2]),
          fromIntegral (hexPair [g1, g2]),
          fromIntegral (hexPair [b1, b2])
        )
    _ -> Nothing
  where
    hexPair = foldl (\acc x -> 16 * acc + digitToInt x) 0

-- Print Colour as RGB string (equivalent to Colour#toString)
colorToString :: Colour -> String
colorToString (Colour r g b) = "rgb(" <> show (round r) <> "," <> show (round g) <> "," <> show (round b) <> ")"

-- Multiply color by transformation matrix
multiply :: Colour -> [Float] -> Colour
multiply Colour {..} m =
  let newR = clamp (r * m !! 0 + g * m !! 1 + b * m !! 2)
      newG = clamp (r * m !! 3 + g * m !! 4 + b * m !! 5)
      newB = clamp (r * m !! 6 + g * m !! 7 + b * m !! 8)
   in Colour newR newG newB

-- Apply hue-rotate
hueRotate :: Float -> Colour -> Colour
hueRotate angle color =
  let radians = angle / 180 * pi
      cosA = cos radians
      sinA = sin radians
      matrix =
        [ 0.213 + cosA * 0.787 - sinA * 0.213,
          0.715 - cosA * 0.715 - sinA * 0.715,
          0.072 - cosA * 0.072 + sinA * 0.928,
          0.213 - cosA * 0.213 + sinA * 0.143,
          0.715 + cosA * 0.285 + sinA * 0.140,
          0.072 - cosA * 0.072 - sinA * 0.283,
          0.213 - cosA * 0.213 - sinA * 0.787,
          0.715 - cosA * 0.715 + sinA * 0.715,
          0.072 + cosA * 0.928 + sinA * 0.072
        ]
   in multiply color matrix

-- Define Solver data type for CSS filter solving
data Solver = Solver {target :: Colour, targetHSL :: (Float, Float, Float)}

-- Example Solver implementation (simplified)
solve :: Solver -> (String, Float)
solve Solver {..} =
  let filters = [50, 20, 3750, 50, 100, 100] -- Example filter values
      filterString = css filters
      lossValue = loss filters
   in (filterString, lossValue)

-- Calculate the loss between the current and target color
loss :: [Float] -> Float
loss _ = 0 -- Placeholder for actual loss calculation

-- Convert filters into CSS string
css :: [Float] -> String
css [invert, sepia, saturate, hueRotate, brightness, contrast] =
  "filter: invert("
    <> show (round invert)
    <> ") sepia("
    <> show (round sepia)
    <> ") saturate("
    <> show (round saturate)
    <> ") hue-rotate("
    <> show (round hueRotate)
    <> ") brightness("
    <> show (round brightness)
    <> ") contrast("
    <> show (round contrast)
    <> ");"
css _ = ""

-- Example usage: Solve for a color
main :: IO ()
main = do
  let hex = "#00a4d6"
  case hexToRgb hex of
    Just (r, g, b) -> do
      let color = Colour r g b
      let solver = Solver color (0, 0, 0) -- Simplified HSL
      let (filterStr, lossVal) = solve solver
      putStrLn $ "CSS Filter: " ++ filterStr
      putStrLn $ "Loss: " ++ show lossVal
    Nothing -> putStrLn "Invalid hex format!"
