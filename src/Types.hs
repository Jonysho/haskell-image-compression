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
  sqrt $ fromIntegral (fromIntegral (r1 - r2)^2 + fromIntegral (g1 - g2)^2 + fromIntegral (b1 - b2)^2)