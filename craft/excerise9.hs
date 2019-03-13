-- @Author: ariesduanmu
-- @Date:   2019-03-13 20:49:50
-- @Last Modified by:   ariesduanmu
-- @Last Modified time: 2019-03-13 21:22:13
fact :: Integer -> Integer
fact n 
  | n==0 = 1
  | otherwise = n * fact (n-1)

mult :: Integer -> Integer -> Integer

mult 0 _ = 0
mult _ 0 = 0
mult a b
  | a >0 = b+(mult (a-1) b)
  | b >0 = a+(mult a (b-1))
  | otherwise = b+(mult (a+1) b)