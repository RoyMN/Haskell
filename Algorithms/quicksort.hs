{- A compact quicksort by divide and
conquer.
------------------------
ex: [1,7,-3,-2,6] -> [-3,-2] ++ [1] ++ [7,6]
                  -> [] ++ [-3] ++ [-2] ++ [1] ++ [6] ++ [7]
                   = [-3,-2,1,6,7] 
-}
qs :: Ord a => [a] -> [a]
qs [] = []
qs (x:xs) = qs [y | y <-xs, y<x] 
            ++ [x] ++ qs [y | y <- xs, y>=x]