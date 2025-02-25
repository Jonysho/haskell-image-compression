module QuadtreeTests where

import Quadtree

-- Test cases
test :: Quadtree -> Quadtree -> Bool
test original expected = blur original == expected

runTest :: String -> Quadtree -> Quadtree -> IO ()
runTest name original expected = do
    let result = test original expected
    putStrLn $ name ++ ": " ++ show result

blurDepth2, blurDepth2Ans :: Quadtree
blurDepth2 = Node
    (Node Black Black White White)
    (Node Black White Black White)  
    (Node Black Black Black Black)  
    (Node Black White Black Black)  

blurDepth2Ans = Node
    (Node Black Black White Black)  
    (Node White Black White Black)  
    (Node Black Black Black Black)  
    (Node White Black Black Black)  

mix, mixAns :: Quadtree
mix = Node
    White 
    (Node Black Black White Black)  
    (Node Black Black Black Black)  
    White
    
mixAns = Node
    Black 
    (Node Black Black Black Black)  
    (Node Black Black Black Black)  
    Black

depth5, depth5Ans :: Quadtree
depth5 = Node
    (Node Black White 
        (Node Black Black 
            (Node White Black White 
                (Node Black Black Black Black)
            )
        Black) 
    White)
    (Node White White White Black)
    White
    White

depth5Ans = Node
    (Node White Black 
        (Node Black Black 
            (Node Black Black Black
                (Node Black Black Black Black)
            )
        Black)
    Black)
    (Node White White White White)
    White
    White

dependent, dependentAns, dependent2, dependentAns2 :: Quadtree
dependent = Node Black (Node Black White White White) White Black
dependentAns = Node Black (Node White White White White) White Black

dependent2 =  Node (Node White Black White White) Black Black White
dependentAns2 = Node (Node White White White White) Black Black White

-- 16x16 Quadtree example
quadtree16x16 :: Quadtree
quadtree16x16 = Node
    (Node (Node Black Black White White) (Node Black White Black White) (Node Black Black Black Black) (Node Black White Black Black))
    (Node (Node White Black White Black) (Node White White Black White) (Node Black Black White White) (Node Black White Black Black))
    (Node (Node Black Black Black Black) (Node Black White Black White) (Node White White Black Black) (Node Black Black Black Black))
    (Node (Node Black White Black Black) (Node Black Black White White) (Node Black White Black White) (Node White Black White Black))

quadtree16x16Ans :: Quadtree
quadtree16x16Ans = Node
    (Node (Node Black Black White Black) (Node White Black Black Black) (Node Black Black Black Black) (Node White Black Black Black))
    (Node (Node Black White White Black) (Node White White White White) (Node White Black White Black) (Node Black Black Black Black))
    (Node (Node Black Black Black Black) (Node White Black White Black) (Node White Black Black Black) (Node Black Black Black Black))
    (Node (Node Black Black Black Black) (Node Black Black White Black) (Node White Black White Black) (Node Black Black Black White))

main :: IO ()
main = do
    runTest "16x16 Quadtree" quadtree16x16 quadtree16x16Ans
    runTest "test dependencies" dependent dependentAns
    runTest "test dependencies" dependent2 dependentAns2
    runTest "Small 1" (Node Black Black White Black) (Node Black Black Black Black)
    runTest "Small 2" (Node Black Black White Black) (Node Black Black Black Black)
    runTest "Small 3" (Node White Black White White) (Node White White White White)
    runTest "Small 4" (Node White Black White Black) (Node Black White Black White)
    runTest "Medium 1" blurDepth2 blurDepth2Ans
    runTest "Mix 1" mix mixAns
    runTest "Depth 5" depth5 depth5Ans