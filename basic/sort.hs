quicksort [] = []
quicksort (x:xs) = quicksort [y | y <- xs, y <= x] ++ [x] ++ (quicksort [y | y <- xs, y > x])

mergeSplit :: [a] -> ([a], [a])
mergeSplit x = (take n x, drop n x)
    where n = div (length x) 2 

mergeMerge x [] = x
mergeMerge [] x = x
mergeMerge (x:xs) (y:ys)
    | (x < y) = x:mergeMerge xs (y:ys)
    | otherwise = y:mergeMerge (x:xs) ys

mergeSort xs
    | (length xs) > 1 = mergeMerge (mergeSort ls) (mergeSort rs)
    | otherwise = xs
    where (ls, rs) = mergeSplit xs

quicksortBy cmp [] = []
quicksortBy cmp (x:xs) = (quicksortBy cmp [y | y<-xs, cmp x y == GT]) ++ [x] ++ (quicksortBy cmp [y | y<-xs, cmp x y == LT])

mergeMergeBy cmp x [] = x
mergeMergeBy cmp [] x = x
mergeMergeBy cmp (x:xs) (y:ys)
    | cmp x y == LT = x:mergeMergeBy cmp xs (y:ys)
    | otherwise = y:mergeMergeBy cmp (x:xs) ys

mergeSortBy cmp xs
    | (length xs) > 1 = mergeMergeBy cmp (mergeSortBy cmp ls) (mergeSortBy cmp rs)
    | otherwise = xs
    where (ls, rs) = mergeSplit xs