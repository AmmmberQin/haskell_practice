-- @Author: ariesduanmu
-- @Date:   2019-03-13 20:49:50
-- @Last Modified by:   ariesduanmu
-- @Last Modified time: 2019-03-14 00:14:48
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


shunt :: [a] -> [a] -> [a]
shunt [] ys = ys
shunt (x:xs) ys = shunt xs (x:ys) 

rev :: [a] -> [a]
rev xs = shunt xs []

-- PROVE 
rev (xs++ys) = rev ys ++ rev xs

-- 1. obveriously works in base (when xs=[])
-- 2. need to prove
-- rev ((x:xs)++ys) = rev ys ++ rev (x:xs)

-- P1.
rev ((x:xs)++ys) = shunt ((x:xs)++ys) []
rev ((x:xs)++ys) = shunt (x:(xs++ys) []
rev ((x:xs)++ys) = shunt (xs++ys) [x]
rev ((x:xs)++ys) = ((shunt (xs++ys) []):x)
rev ((x:xs)++ys) = (rev ys ++ rev xs):x)
rev ((x:xs)++ys) = rev ys ++ rev xs ++ [x]
rev ((x:xs)++ys) = rev ys ++ (shunt xs [] ++ [x])
rev ((x:xs)++ys) = rev ys ++ (shunt xs [x])
rev ((x:xs)++ys) = rev ys ++ (shunt (x:xs) [])
rev ((x:xs)++ys) = rev ys ++ rev (x:xs)


