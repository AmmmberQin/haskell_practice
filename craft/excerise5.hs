import Test.HUnit

maxOccurs :: Integer -> Integer -> (Integer, Integer)
maxOccurs a b
  | a==b = (a, 2)
  | otherwise = ((max a b), 1)

maxThreeOccurs :: Integer -> Integer -> Integer -> (Integer, Integer)
maxThreeOccurs a b c
  | j == 2 = (n, (j+i-1))
  | j == 1 = (n, 1)
  where (m, i) = maxOccurs a b
        (n, j) = maxOccurs m c

maxThree :: Integer -> Integer -> Integer -> Integer
maxThree a b c = max (max a b) c

minThree :: Integer -> Integer -> Integer -> Integer
minThree a b c = min (min a b) c

middle :: Integer -> Integer -> Integer -> Integer
middle a b c = (a+b+c) - (i+j)
  where i = minThree a b c
        j = maxThree a b c

orderTriple :: (Integer, Integer, Integer) -> (Integer, Integer, Integer)
orderTriple (a, b, c) = (i, j, k)
  where i = minThree a b c
        j = middle a b c
        k = maxThree a b c

accrossX :: (Float, Float) -> Float
accrossX (a, b)
  | a == 0 = 0
  | otherwise = (-1)*b / a 