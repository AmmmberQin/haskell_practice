-- It is fault while negtive
floor_clever_dick :: Float -> Integer
floor_clever_dick = read . takeWhile (/= '.') . show

iuntil :: (a -> Bool) -> (a -> a) -> a -> a
iuntil p f x = if p x then x else iuntil p f (f x)

m `leq` n = fromInteger m <= n
m `lt` n = m < fromInteger n

ifloor x
    | x<0 = until (`leq` x) (subtract 1) (-1)
    | x>=0 = until (x `lt`) (+1) 1 - 1

bfloor x = fst (until unit (shrink x) (bound x))
    where unit (m, n) = (m+1 == n)

lower x = until (`leq` x) (*2) (-1)
upper x = until (x `lt`) (*2) 1
bound x = (lower x, upper x)

choose (m,n) = (m+n) `div` 2

shrink x (m,n)
    | p `leq` x = (p, n)
    | otherwise = (m, p)
    where p = choose (m, n)
