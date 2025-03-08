module Base where

-- Define quadtree with black, white node or node with 4 children
data Quadtree where
  Black :: Quadtree
  White :: Quadtree
  Node :: Quadtree -> Quadtree -> Quadtree -> Quadtree -> Quadtree
  deriving (Eq, Show)

allBlack, allWhite :: Int -> Quadtree
allBlack _ = Black
allWhite _ = White

clockwise, anticlockwise :: Quadtree -> Quadtree -> Quadtree -> Quadtree -> Quadtree
clockwise a b c d = Node a b c d
anticlockwise a b c d = Node a d c b

type BBox = (Double, Double, Double, Double)  -- xmin, ymin, xmax, ymax

-- Main blur function: return Blurred Quadtree
blur :: Quadtree -> Quadtree
blur qt = blur' qt (0,0,1,1) leaves
  where
    leaves = getAllLeaves qt (0,0,1,1)

-- Recursive blur helper with global context
blur' :: Quadtree -> BBox -> [(Quadtree, BBox)] -> Quadtree -- Quadtree to blur, bbox and all leaves -> blurred Quadtree
-- If leaf, return the leaf
blur' Black _ _ = Black
blur' White _ _ = White
-- If node, process children
blur' (Node a b c d) bbox allLeaves =
    let (x0, y0, x1, y1) = bbox
        midX = (x0 + x1) / 2
        midY = (y0 + y1) / 2
        
        -- Process children with correct bounding boxes
        a' = blur' a (x0, midY, midX, y1) allLeaves -- top left
        b' = blur' b (midX, midY, x1, y1) allLeaves -- top right
        c' = blur' c (midX, y0, x1, midY) allLeaves -- bottom right
        d' = blur' d (x0, y0, midX, midY) allLeaves -- bottom left
        
        -- Find neighbors for each leaf
        aNeighbors = findEdgeNeighbors (x0, midY, midX, y1) allLeaves
        bNeighbors = findEdgeNeighbors (midX, midY, x1, y1) allLeaves
        cNeighbors = findEdgeNeighbors (midX, y0, x1, midY) allLeaves
        dNeighbors = findEdgeNeighbors (x0, y0, midX, midY) allLeaves

        -- Apply blur rules based on neighbors, which flips the color if necessary
        a'' = blurLeaf a' aNeighbors
        b'' = blurLeaf b' bNeighbors
        c'' = blurLeaf c' cNeighbors
        d'' = blurLeaf d' dNeighbors
    in Node a'' b'' c'' d''

-- Collect all leaves in the tree with their bounding boxes
getAllLeaves :: Quadtree -> BBox -> [(Quadtree, BBox)]
-- Base cases
getAllLeaves Black bbox = [(Black, bbox)]
getAllLeaves White bbox = [(White, bbox)]
-- Concats all leaves from children
getAllLeaves (Node a b c d) bbox =
    let (x0, y0, x1, y1) = bbox
        midX = (x0 + x1) / 2
        midY = (y0 + y1) / 2
    in getAllLeaves a (x0, midY, midX, y1) ++ 
       getAllLeaves b (midX, midY, x1, y1) ++ 
       getAllLeaves c (midX, y0, x1, midY) ++ 
       getAllLeaves d (x0, y0, midX, midY)    

-- Neighbor detection with edge cases handling
findEdgeNeighbors :: BBox -> [(Quadtree, BBox)] -> [Quadtree]
findEdgeNeighbors target@(x0,y0,x1,y1) leaves =
    let tiny = 1e-9
        
        -- Get boundaries of target bounding box, adding tiny to expand it slightly
        rightEdge  = (x1 - tiny, y0, x1 + tiny, y1)
        leftEdge   = (x0 - tiny, y0, x0 + tiny, y1)
        topEdge    = (x0, y1 - tiny, x1, y1 + tiny)
        bottomEdge = (x0, y0 - tiny, x1, y0 + tiny)
        
        -- Bounding box intersection check helper function, returns true if intersect
        intersects (ax0, ay0, ax1, ay1) (bx0, by0, bx1, by1) =
            not (ax1 <= bx0 || ax0 >= bx1 || ay1 <= by0 || ay0 >= by1)
        
        -- Check if neighbor touches any edge
        isNeighbor (_, nb) = any (intersects nb) [rightEdge, leftEdge, topEdge, bottomEdge]
        
        -- Remove self from neighbors
        isNotSelf (_, nb) = nb /= target

        -- Filter only for neighbors that touch any edge
        neighbors = filter isNeighbor leaves
        
        -- Filter out the target itself
        filteredNeighbors = filter isNotSelf neighbors
        
        -- Extract the Quadtree nodes from the filtered list
        result = map fst filteredNeighbors
    in result

-- Blur rules with strict majority check
blurLeaf :: Quadtree -> [Quadtree] -> Quadtree
blurLeaf Black neighbors
    | whiteCount > blackCount = White -- Flip if more white than black
    | otherwise = Black
  where
    whiteCount = count White neighbors
    blackCount = count Black neighbors

blurLeaf White neighbors
    | blackCount > whiteCount = Black -- Flip if more black than white
    | otherwise = White
  where
    whiteCount = count White neighbors
    blackCount = count Black neighbors
blurLeaf node _ = node 

-- Returns the count of a specific color in a list of Quadtree
count :: Quadtree -> [Quadtree] -> Int
count color = length . filter (== color) -- Count the number of colors in the list
