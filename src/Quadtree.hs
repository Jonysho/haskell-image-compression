module Quadtree (
    buildQuadtree,
    isUniform,
    splitQuadrants
) where

import Types
import Data.List (transpose)

-- Calculate average color and standard deviation for a region
regionProperties :: [[RGB]] -> (RGB, Double)
regionProperties pixels =
  let (rs, gs, bs) = unzip3 (concat pixels)
      len = fromIntegral (length rs)
      avg xs = fromIntegral (sum (map fromIntegral xs)) `div` len
      avgColor = (avg rs, avg gs, avg bs)
      stdDev = sqrt (sum [colorDistance avgColor px | px <- concat pixels] / fromIntegral len)
  in (avgColor, stdDev)

-- Check if a region is uniform
isUniform :: [[RGB]] -> Double -> Bool
isUniform pixels threshold =
  let (_, stdDev) = regionProperties pixels
  in stdDev <= threshold

-- Split a region into 4 quadrants
splitQuadrants :: [[RGB]] -> ([[RGB]], [[RGB]], [[RGB]], [[RGB]])
splitQuadrants pixels =
  let rows = length pixels
      cols = length (head pixels)
      (top, bottom) = splitAt (rows `div` 2) pixels
      splitRow row = splitAt (cols `div` 2) row
      (tl, tr) = unzip (map splitRow top)
      (bl, br) = unzip (map splitRow bottom)
  in (tl, tr, bl, br)

-- Build the quadtree
buildQuadtree :: [[RGB]] -> BBox -> Double -> Int -> Quadtree
buildQuadtree pixels (x, y, w, h) threshold depth
  | isUniform pixels threshold || depth >= maxDepth = Leaf (x, y, w, h) avgColor
  | otherwise =
      let (tl, tr, bl, br) = splitQuadrants pixels
          halfW = w `div` 2
          halfH = h `div` 2
      in Node (x, y, w, h)
              (buildQuadtree tl (x, y, halfW, halfH) threshold (depth + 1))
              (buildQuadtree tr (x + halfW, y, halfW, halfH) threshold (depth + 1))
              (buildQuadtree bl (x, y + halfH, halfW, halfH) threshold (depth + 1))
              (buildQuadtree br (x + halfW, y + halfH, halfW, halfH) threshold (depth + 1))
  where
    (avgColor, _) = regionProperties pixels
    maxDepth = 8  -- Arbitrary limit to prevent infinite recursion