-- @Author: ariesduanmu
-- @Date:   2019-02-26 00:21:44
-- @Last Modified by:   ariesduanmu
-- @Last Modified time: 2019-02-26 01:01:49
imax a b
  | a>=b = a
  | otherwise = b

maxThree a b c = imax (imax a b) c

maxFour1 :: Integer -> Integer -> Integer -> Integer -> Integer

maxFour1 a b c d = maxThree (maxThree a b c) c d

maxFour2 :: Integer -> Integer -> Integer -> Integer -> Integer

maxFour2 a b c d = imax (imax a b) (imax c d)

maxFour3 :: Integer -> Integer -> Integer -> Integer -> Integer

maxFour3 a b c d = imax (maxThree a b c) d

between :: Integer -> Integer -> Integer -> Bool

between a b c = b >= a && c >= b 

howManyEqual :: Integer -> Integer -> Integer -> Integer

howManyEqual a b c
  | (a == b && a == c) = 3
  | (a == b || a == c || b == c) = 2
  | otherwise = 1

howManyOfFourEqual :: Integer -> Integer -> Integer -> Integer -> Integer

howManyOfFourEqual a b c d
  | ((howManyEqual a b c) == 3 && a == d) = 4
  | ((howManyEqual a b c) == 1) = (howManyEqual b c d)
  | otherwise = (maxFour1 (howManyEqual a b c) (howManyEqual a c d) (howManyEqual a b d) (howManyEqual b c d))