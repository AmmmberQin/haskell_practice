-- @Author: ariesduanmu
-- @Date:   2019-02-28 00:19:43
-- @Last Modified by:   ariesduanmu
-- @Last Modified time: 2019-02-28 01:07:46
fac :: Integer -> Integer
fac n
  | n == 0 = 1
  | n > 0 = fac (n-1) * n
  | otherwise =  error "fac only defined on natural numbers"


rangeProduct :: Integer -> Integer -> Integer

rangeProduct m n
  | m > n = 0
  | m == n = m
  | otherwise = rangeProduct (m+1) n * m 

fac1 :: Integer -> Integer
fac1 n
  | n == 0 =1
  | n > 0 = rangeProduct 1 n
  | otherwise = error "fac only defined on natural numbers"

regions :: Integer -> Integer
regions n
  | n == 0 = 1
  | n > 0 = regions (n-1) + n

mulFacs :: Integer -> Integer
mulFacs n
  | n == 0 = 0
  | n == 1 = 1
  | n > 0 = mulFacs (n-1) * n

sqrtNatural :: Integer -> Integer
sqrtNatural n
  | n == 0 = 0
  | n > 0 && (m+1)*(m+1) <= n = m+1
  | n > 0 = m
  | otherwise = error "sqrt only defined on natural numbers"
  where m = sqrtNatural (n-1)

f :: Integer -> Integer
f 0 = 0
f 1 = 44
f 2 = 17
f _ = 0

maxF :: Integer -> Integer
maxF n
  | n == 0 = f 0
  | n > 0 = max (maxF (n-1)) (f n) 

zeroIn :: Integer -> Bool
zeroIn n
  | n == 0 = f 0 == 0
  | n > 0 = zeroIn (n-1) || f n == 0


sumFun :: (Integer -> Integer) -> Integer -> Integer
sumFun f n
  | n == 0 = f 0
  | n > 0 = sumFun f (n-1) + f n

r :: Integer -> Integer
r n
  | n == 0 = 1
  | otherwise = n

regions1 :: Integer -> Integer
regions1 n = sumFun r n