module Quadtree (
    buildQuadtree,
    isUniform,
    getRegionPixels
) where

import Types
import qualified Data.Vector as V
import Data.Word (Word8)

-- Get pixels for a specific region from the Vector
getRegionPixels :: V.Vector RGB -> BBox -> Int -> V.Vector RGB
getRegionPixels allPixels (x, y, w, h) fullWidth =
  V.generate (w * h) (\i ->
      let px = x + i `mod` w
          py = y + i `div` w
          idx = py * fullWidth + px
      in if idx < V.length allPixels
         then allPixels V.! idx
         else (0, 0, 0)  -- Fallback for out-of-bounds, though should not occur
  )

-- Calculate average color and variance for a region
regionProperties :: V.Vector RGB -> (RGB, Double)
regionProperties pixels =
  let pixelCount = V.length pixels
      (sumR, sumG, sumB) = V.foldr' (\(r, g, b) (sr, sg, sb) -> 
                             (sr + fromIntegral r, 
                              sg + fromIntegral g, 
                              sb + fromIntegral b)) 
                           (0, 0, 0) pixels
                           
      avgR = round (sumR / fromIntegral pixelCount) :: Word8
      avgG = round (sumG / fromIntegral pixelCount) :: Word8
      avgB = round (sumB / fromIntegral pixelCount) :: Word8
      avgColor = (avgR, avgG, avgB)
      
      -- Calculate variance as average squared distance (without sqrt for efficiency)
      variance = V.foldl' (\acc px -> acc + colorDistanceSquared avgColor px) 0 pixels 
                 / fromIntegral pixelCount
  in (avgColor, variance)

-- Squared distance between two colors for variance calculation
colorDistanceSquared :: RGB -> RGB -> Double
colorDistanceSquared (r1, g1, b1) (r2, g2, b2) =
  let dr = fromIntegral r1 - fromIntegral r2
      dg = fromIntegral g1 - fromIntegral g2
      db = fromIntegral b1 - fromIntegral b2
  in dr*dr + dg*dg + db*db

-- Check uniformity using squared variance to avoid sqrt
isUniform :: V.Vector RGB -> Double -> Bool
isUniform pixels threshold =
  let (_, variance) = regionProperties pixels
  in variance <= threshold || V.length pixels <= 1

-- Update buildQuadtree to use Vector
buildQuadtree :: V.Vector RGB -> Int -> BBox -> Double -> Int -> Int -> Quadtree
buildQuadtree allPixels fullWidth (x, y, w, h) threshold maxDepth currentDepth
  | currentDepth >= maxDepth || w <= 1 || h <= 1 || isUniform regionPixels threshold =
      Leaf (x, y, w, h) avgColor
  | otherwise =
      let halfW = max 1 (w `div` 2)
          halfH = max 1 (h `div` 2)
          remW = w - halfW
          remH = h - halfH
          
          tlBBox = (x, y, halfW, halfH)
          trBBox = (x + halfW, y, remW, halfH)
          blBBox = (x, y + halfH, halfW, remH)
          brBBox = (x + halfW, y + halfH, remW, remH)
          
          tl = buildQuadtree allPixels fullWidth tlBBox threshold maxDepth (currentDepth + 1)
          tr = buildQuadtree allPixels fullWidth trBBox threshold maxDepth (currentDepth + 1)
          bl = buildQuadtree allPixels fullWidth blBBox threshold maxDepth (currentDepth + 1)
          br = buildQuadtree allPixels fullWidth brBBox threshold maxDepth (currentDepth + 1)
      in Node (x, y, w, h) tl tr bl br
  where
    regionPixels = getRegionPixels allPixels (x, y, w, h) fullWidth
    (avgColor, _) = regionProperties regionPixels
