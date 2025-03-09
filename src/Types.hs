module Types where

import Data.Word (Word8)

-- RGB color representation
type RGB = (Word8, Word8, Word8)

-- Bounding box (x, y, width, height)
type BBox = (Int, Int, Int, Int)

-- Quadtree structure
data Quadtree
  = Leaf BBox RGB          -- Leaf node: bounding box and average color
  | Node BBox Quadtree Quadtree Quadtree Quadtree  -- Internal node: 4 children
  deriving (Show)

-- Euclidean distance between two colors
colorDistance :: RGB -> RGB -> Double
colorDistance (r1, g1, b1) (r2, g2, b2) =
  let r = fromIntegral r1 - fromIntegral r2
      g = fromIntegral g1 - fromIntegral g2
      b = fromIntegral b1 - fromIntegral b2
  in sqrt (r*r + g*g + b*b)