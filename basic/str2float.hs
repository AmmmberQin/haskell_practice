filterStr [] = []
filterStr (x:xs)
    | x `elem` ['0'..'9'] || x == '.' = x:filterStr xs
    | otherwise = filterStr xs

str2float xs = read (filterStr xs) :: Float
