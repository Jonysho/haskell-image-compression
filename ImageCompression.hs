{-# LANGUAGE FlexibleContexts #-}
module ImageCompression where

import Codec.Picture (readImage, generateImage, savePngImage, PixelRGB8(..), Image, DynamicImage(..), convertRGB8, imageWidth, imageHeight, pixelAt)
import Data.Word (Word8)
import Quadtree (Quadtree(..), blur)

data ImageQuadTree = ILeaf PixelRGB8 | INode ImageQuadTree ImageQuadTree ImageQuadTree ImageQuadTree

toImageQuadTree :: Quadtree -> ImageQuadTree
toImageQuadTree Black = ILeaf (PixelRGB8 0 0 0)
toImageQuadTree White = ILeaf (PixelRGB8 255 255 255)
toImageQuadTree (Node a b c d) = INode (toImageQuadTree a) (toImageQuadTree b) (toImageQuadTree c) (toImageQuadTree d)

imageToQuadtree :: Image PixelRGB8 -> Int -> Int -> Int -> Int -> Quadtree
imageToQuadtree img x y width height
  | isHomogeneous img x y width height = Black
  | otherwise = Node (imageToQuadtree img x y halfW halfH)
                     (imageToQuadtree img (x + halfW) y halfW halfH)
                     (imageToQuadtree img x (y + halfH) halfW halfH)
                     (imageToQuadtree img (x + halfW) (y + halfH) halfW halfH)
  where
    halfW = width `div` 2
    halfH = height `div` 2
    isHomogeneous img x y width height =
      let pixel (i, j) = pixelAt img (x + i) (y + j)
          firstPixel = pixel (0, 0)
      in all (== firstPixel) [pixel (i, j) | i <- [0..width-1], j <- [0..height-1]]

compressImage :: FilePath -> FilePath -> IO ()
compressImage inputPath outputPath = do
  eitherImg <- readImage inputPath
  case eitherImg of
    Left err -> error $ "Failed to load image: " ++ err
    Right img -> do
      let rgbImg = convertRGB8 img
          width = imageWidth rgbImg
          height = imageHeight rgbImg
          qt = imageToQuadtree rgbImg 0 0 width height
          blurredQt = blur qt
          compressedImg = quadtreeToImage (toImageQuadTree blurredQt) width height
      savePngImage outputPath (ImageRGB8 compressedImg)

quadtreeToImage :: ImageQuadTree -> Int -> Int -> Image PixelRGB8
quadtreeToImage qt width height = generateImage (getPixel qt) width height
  where
    getPixel (ILeaf color) _ _ = color
    getPixel (INode nw ne sw se) x y =
      let halfW = width `div` 2
          halfH = height `div` 2
      in if x < halfW && y < halfH then getPixel nw x y
         else if x >= halfW && y < halfH then getPixel ne (x - halfW) y
         else if x < halfW && y >= halfH then getPixel sw x (y - halfH)
         else getPixel se (x - halfW) (y - halfH)
