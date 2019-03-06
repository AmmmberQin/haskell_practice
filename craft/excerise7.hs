-- @Author: ariesduanmu
-- @Date:   2019-03-06 01:36:11
-- @Last Modified by:   ariesduanmu
-- @Last Modified time: 2019-03-06 01:55:23
module Chapter7 where

import Data.Char
import Data.List
import Test.QuickCheck
import Test.HUnit

digits :: String -> String
digits st = [ch | ch <- st, isDigit ch]

firstDigit :: String -> Char
firstDigit st
  = case (digits st) of
    []      -> '\0'
    (x:_)   -> x

firstDigitNumber :: String -> Integer
firstDigitNumber st
  = case (digits st) of 
    [] -> 0
    (x:_) -> (read [x]) + 1

weridMatch :: String -> String
weridMatch st
  = case (digits st) of 
    (x:y:_) -> show ((read [x]) + (read [y]))
    (x:_)   -> [(head st)]
    []      -> "0"

product1 :: [Integer] -> Integer

product1 ns
  = case ns of 
    []     -> 1
    (x:xs) -> product1 xs * x

add1 :: [Bool] -> Bool

add1 bs
  = case bs of 
    []     -> True
    (x:xs) -> add1 xs && x

or1 :: [Bool] -> Bool
or1 bs
  = case bs of 
    []     -> True
    (x:xs) -> or1 xs || x

--- sort ---
iSort :: [Integer] -> [Integer]
iSort [] = []
iSort (x:xs) = ins x (iSort xs)


ins :: Integer -> [Integer] -> [Integer]

ins x [] = [x]
ins x (y:ys)
  | x<=y = x:(y:ys)
  | otherwise = y:(ins x ys)

prop_sort xs = iSort xs == sort xs

testIns1 = TestCase(assertEqual "for: ins 6 [1,2,3]" [1,2,3,6] (ins 6 [1,2,3]))
testIns2 = TestCase(assertEqual "for: ins 1 [2,3,6]" [1,2,3,6] (ins 1 [2,3,6]))
testIns3 = TestCase(assertEqual "for: ins 2 [1,3,6]" [1,2,3,6] (ins 2 [1,3,6]))
testIns4 = TestCase(assertEqual "for: ins 3 [1,3,6]" [1,3,3,6] (ins 3 [1,3,6]))

testsIns = TestList [testIns1, testIns2, testIns3, testIns4]

--- reverse sort --- 
iSort1 :: [Integer] -> [Integer]
iSort1 [] = []
iSort1 (x:xs) = ins1 x (iSort1 xs)

ins1 :: Integer -> [Integer] -> [Integer]

ins1 x [] = [x]
ins1 x (y:ys)
  | x<y = y:(ins1 x ys)
  | otherwise = x:(y:ys)

prop_reverse_sort xs = iSort1 xs == reverse (sort xs)

--- remove duplicate sort ---
iSort2 :: [Integer] -> [Integer]
iSort2 [] = []
iSort2 (x:xs) = ins2 x (iSort2 xs)

ins2 :: Integer -> [Integer] -> [Integer]
ins2 x [] = [x]
ins2 x (y:ys)
  | x<y = x:y:ys
  | x==y = y:ys
  | otherwise = y:(ins2 x ys)

removeDuplicates :: [Integer] -> [Integer]
removeDuplicates [] = []
removeDuplicates (x:xs)
  | elem x xs = removeDuplicates xs
  | otherwise = x:(removeDuplicates xs)

prop_remove_duplcate_sort xs = iSort2 xs == sort (removeDuplicates xs) 

--- lexicographic sort ---
iSort3 :: [(Integer, Integer)] -> [(Integer, Integer)]
iSort3 [] = []
iSort3 (x:xs) = ins3 x (iSort3 xs)

ins3 :: (Integer, Integer) -> [(Integer, Integer)] -> [(Integer, Integer)]
ins3 x [] = [x]
ins3 x (y:ys)
  | ((fst x)<(fst y)) || ((fst x)==(fst y) && (snd x)<=(snd y)) = x:y:ys
  | otherwise = y:(ins3 x ys)

prop_tuple_sort xs = iSort3 xs == sort xs

elemNum :: Integer -> [Integer] -> Integer
elemNum n [] = 0
elemNum n (x:xs)
  | n==x = 1 + (elemNum n xs)
  | otherwise = elemNum n xs

elemNum1 :: Integer -> [Integer] -> Integer
elemNum1 n xs = toInteger (length [x | x<-xs, x==n])

unique :: [Integer] -> [Integer]
unique xs = [x | x<-xs, elemNum x xs == 1]

prop_num_unique xs = and [(elemNum1 n xs) == 1 | n<-(unique xs)]

reverse1 :: [a] -> [a]
reverse1 [] = []
reverse1 (x:xs) = reverse1 xs ++ [x]

unzip1 :: [(a,b)] -> ([a], [b])
unzip1 [] = ([], [])
unzip1 (x:xs) = (i:a, j:b)
  where (a, b) = unzip1 xs
        (i, j) = x

min1 :: [Integer] -> Integer
min1 xs = head (iSort xs)

min2 :: [Integer] -> Integer
min2 (x:xs)
  | xs == [] = x
  | otherwise = min x (min2 xs)

max1 :: [Integer] -> Integer
max1 xs = last (iSort xs)

max2 :: [Integer] -> Integer
max2 (x:xs)
  | xs == [] = x
  | otherwise = max x (max2 xs)

isSorted :: [Integer] -> Bool

isSorted [] = True
isSorted (x:[]) = True
isSorted (x:y:ys)
  | x > y = False
  | otherwise = isSorted (y:ys)

prop_is_sort xs = isSorted (iSort xs)
prop_ins_is_sort n xs = isSorted (ins n (iSort xs))

qSort :: [Integer] -> [Integer]
qSort [] = []
qSort (x:xs) = qSort [y|y<-xs, y<=x] ++ [x] ++ qSort [y|y<-xs, y>x]

prop_qSort xs = qSort xs == sort xs

drop1 :: Int -> [a] -> [a]
drop1 0 xs = xs
drop1 _ [] = []
drop1 n (x:xs)
  | n > 0 = drop1 (n-1) xs
drop1 _ _ = error "PreludeList.drop: negative arguments"

prop_drop n xs = drop1 n xs == drop n xs

splitAt1 :: Int -> [a] -> [a]
splitAt1