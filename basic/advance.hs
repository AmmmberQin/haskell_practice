(\x -> x ++ " def") "abc"

map (\x -> x * x) [1, 2, 3]

map (+ 1) [1,2,3]

filter odd [1,2,3,4,5]

foldl (\x c -> c + x) 0 [1..10]

foldl1 (+) [1..3]

-- 柯里化