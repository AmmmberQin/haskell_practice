m `leq` x = fromInteger m <= x
x `lt` n  = x < fromInteger n

floor1 x
    | x < 0 = until (`leq` x) (subtract 1) (-1)
    | otherwise = until (x `lt`) (+1) 1 - 1

floor2 :: Float -> Integer
floor2 x = fst (until unit (shrink x) (bound x))

unit (m,n) = (m+1 == n)

type Interval = (Integer,Integer)

shrink :: Float -> Interval -> Interval
shrink x (m, n)
    | p `leq` x = (p, n)
    | otherwise = (m, p)
    where p = choose (m,n)

choose (m,n) = (m+n) `div` 2

bound :: Float -> Interval
bound x = (lower x, upper x)

lower x = until (`leq` x) (*2) (-1)

upper x = until (x `lt`) (*2) 1