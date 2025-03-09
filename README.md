# Image Compression using Quadtree

This project compresses images using a **Quadtree** algorithm, reducing file size while maintaining visual quality. Written in **Haskell**, it uses the `JuicyPixels` library for image processing.

---

## Features

- **Quadtree-based compression**: Recursively divides images into regions for efficient compression.
- **Customizable**: Adjust the compression threshold and quadtree depth.
- **Lossy compression**: Reduces file size with minimal quality loss.

## How It Works

1. **Read the Image**:
   - The program reads the input image (e.g., `car.png`) and converts it into a grid of RGB pixels.

2. **Build the Quadtree**:
   - The image is recursively divided into four regions (quadrants).
   - If the pixels in a region are similar (based on the threshold), the region is represented by a single average color.
   - If not, the region is further divided into smaller quadrants until the maximum depth is reached.

3. **Reconstruct the Image**:
   - The quadtree is traversed to reconstruct the compressed image, replacing regions with their average colors.

4. **Save the Compressed Image**:
   - The compressed image is saved as a new PNG file (e.g., `car-compressed.png`).
---

| Original Image (`car.png`) | Compressed Image (`car-compressed.png`) |
|---------------------------|----------------------------------|
| ![Original Image](car.png) **(423 KB)** | ![Compressed Image](car-compressed.png) **(133 KB)** |

---

## Usage

Build the project and run:
   ```bash
   cabal build
   cabal run image-compression -- <image-path> <threshold> <max-depth>
E.g. cabal run image-compression -- car.png 10.0 5
```
The compressed image will be saved as <image-name>-compressed.png
