myRange start step = start:(myRange (start+step) step)

xRange x = myRange x 2
yRange y = myRange y 4

zRange x y = x+y:(myRange (x+y+7) 7)

half x = map (`div` 2) x
addReturn x = map (++"\n") x

primes = sieve (2:[3,5..])
    where sieve (x:xs) = x:sieve [a | a <- xs, mod a x > 0]

inc a = a+1
cropStr s n m
    | (length s) <= n = (show m)++". "++s
    | otherwise = (show m)++". "++(take n s)++"\n"++(cropStr (drop n s) n (inc m))