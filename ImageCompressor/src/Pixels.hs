module Pixels where

import Lib
import Vectors

addPixelsValue :: PixelValue -> PixelValue -> PixelValue
addPixelsValue (r1, g1, b1) (r2, g2, b2) = (r1 + r1, g1 + g2, b1 + b2)

computePixelsSum' :: [PixelUnit] -> PixelValue -> PixelValue
computePixelsSum' xs acc
  = foldl (\ acc x -> addPixelsValue acc (value x)) acc xs


computePixelsSum :: [PixelUnit] -> PixelValue
computePixelsSum [] = (0, 0, 0)
computePixelsSum pixels = computePixelsSum' pixels (0, 0, 0)

dividePixelBy :: PixelValue -> Int -> PixelValue
dividePixelBy (r, g, b) v = (div r v, div g v, div b v)

computePixelsMean :: [PixelUnit] -> PixelValue
computePixelsMean pixels = dividePixelBy (computePixelsSum pixels)
 (length pixels)

pixelToString :: PixelUnit -> String
pixelToString (PixelUnit pos rgb) = vector2iToString pos
 ++ " " ++ vector3iToString rgb

pixelsToString :: [PixelUnit] -> String
pixelsToString [] = ""
pixelsToString (pixel:xs) = pixelToString pixel ++ "\n" ++ pixelsToString xs