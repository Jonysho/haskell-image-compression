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