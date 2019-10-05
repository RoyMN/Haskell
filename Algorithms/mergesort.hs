-- Splits a list at the middle.
split :: [a] -> ([a], [a])
split [] = ([], [])
split xs = let h = (length xs) `div` 2 in (take h xs, drop h xs)

-- Combines two sorted lists into one sorted list.
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs)(y:ys) = if x<y then x:(merge xs (y:ys))
                     else y:(merge ys (x:xs))

{-Mergesort by divide and conquer. An empty
list and a list containing just one element
is sorted, and can be merged. All other lists
will recursively be reduced into singletons, before
merging.-}
mergeSort :: Ord a => [a] -> [a]                     
mergeSort [] = []
mergeSort xs = if length xs == 1 then xs
               else merge (mergeSort left) (mergeSort right)
               where (left, right) = split xs