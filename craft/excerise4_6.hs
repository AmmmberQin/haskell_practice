-- @Author: ariesduanmu
-- @Date:   2019-02-28 01:52:53
-- @Last Modified by:   ariesduanmu
-- @Last Modified time: 2019-02-28 02:03:41
gcd1 :: Integer -> Integer -> Integer

gcd1 a b
  | b == 0 = a
  | otherwise = gcd1 b (mod a b)

power2 :: Integer -> Integer
power2 n
  | n <= 0 = 1
  | n == 1 = 2
  | mod n 2 == 0 = (power2 (toInteger (div n 2))) ^ 2
  | otherwise = (power2 (toInteger (div n 2)) ^ 2) * 2