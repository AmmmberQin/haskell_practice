allEven :: [Integer] -> [Integer]
allEven [] = []
allEven (h:t)
    | even h = h:allEven t
    | otherwise = allEven t 