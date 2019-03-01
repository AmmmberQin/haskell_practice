import Test.HUnit
import Test.QuickCheck
import Data.List

maxThree :: Integer -> Integer -> Integer -> Integer
maxThree a b c = max (max a b) c

testMax1 = TestCase(assertEqual "for: maxThree 6 4 1" 6 (maxThree 6 4 1))
testMax2 = TestCase(assertEqual "for: maxThree 6 6 6" 6 (maxThree 6 6 6))
testMax3 = TestCase(assertEqual "for: maxThree 2 6 6" 6 (maxThree 2 6 6))
testMax4 = TestCase(assertEqual "for: maxThree 2 2 6" 6 (maxThree 2 2 6))

testsMax = TestList [testMax1, testMax2, testMax3, testMax4]


mysteryMax :: Integer -> Integer -> Integer -> Integer
mysteryMax x y z
  |x>y && x>z = x
  |y>x && y>z = y
  |otherwise = z

testMax5 = TestCase(assertEqual "for: mysteryMax 6 6 2" 6 (mysteryMax 6 6 2))

testsMMax = TestList [testMax1, testMax2, testMax3, testMax4, testMax5]


fact :: Int -> Int
fact n
  | n>1 = n * fact(n-1)
  | otherwise = 1

prop_fact n = fact n > 0

allEqual :: Integer -> Integer -> Integer -> Bool
allEqual a b c = (a==b && b==c)

testEqual1 = TestCase(assertEqual "for allEqual 1 1 1" True (allEqual 1 1 1))
testEqual2 = TestCase(assertEqual "for allEqual 1 1 2" False (allEqual 1 1 2))
testEqual3 = TestCase(assertEqual "for allEqual 1 2 1" False (allEqual 1 2 1))
testEqual4 = TestCase(assertEqual "for allEqual 2 1 1" False (allEqual 2 1 1))
testEqual5 = TestCase(assertEqual "for allEqual 1 3 2" False (allEqual 1 3 2))

testsEqual = TestList [testEqual1, testEqual2, testEqual3, testEqual4, testEqual5]

-- runTestTT testsEqual

solution :: Integer -> Integer -> Integer -> Bool
solution m n p = ((m+n+p) == 3*p)

testSolution1 = TestCase(assertEqual "for solution 1 1 1" True (solution 1 1 1))
testSolution2 = TestCase(assertEqual "for solution 1 1 2" False (solution 1 1 2))
testSolution3 = TestCase(assertEqual "for solution 1 2 1" False (solution 1 2 1))
testSolution4 = TestCase(assertEqual "for solution 2 1 1" False (solution 2 1 1))
testSolution5 = TestCase(assertEqual "for solution 1 3 2" False (solution 1 3 2))

testsSolution = TestList [testSolution1, testSolution2, testSolution3, testSolution4, testSolution5]

allDifferent :: Integer -> Integer -> Integer -> Bool
allDifferent a b c = (a /= b && b /= c && a /= c)

testDiff1 = TestCase(assertEqual "for allDifferent 1 1 1" False (allDifferent 1 1 1))
testDiff2 = TestCase(assertEqual "for allDifferent 1 1 2" False (allDifferent 1 1 2))
testDiff3 = TestCase(assertEqual "for allDifferent 1 2 1" False (allDifferent 1 2 1))
testDiff4 = TestCase(assertEqual "for allDifferent 2 1 1" False (allDifferent 2 1 1))
testDiff5 = TestCase(assertEqual "for allDifferent 1 3 2" True (allDifferent 1 3 2))

testsDiff = TestList [testDiff1, testDiff2, testDiff3, testDiff4, testDiff5]

attempt m n p = (m/=n) && (n/=p)

testAttempt1 = TestCase(assertEqual "for attempt 1 1 1" False (attempt 1 1 1))
testAttempt2 = TestCase(assertEqual "for attempt 1 1 2" False (attempt 1 1 2))
testAttempt3 = TestCase(assertEqual "for attempt 1 2 1" False (attempt 1 2 1))
testAttempt4 = TestCase(assertEqual "for attempt 2 1 1" False (attempt 2 1 1))
testAttempt5 = TestCase(assertEqual "for attempt 1 3 2" True (attempt 1 3 2))

testsAttempt = TestList [testAttempt1, testAttempt2, testAttempt3, testAttempt4, testAttempt5]


averageThree :: Integer -> Integer -> Integer -> Float
averageThree a b c = (fromInteger a + fromInteger b + fromInteger c) / 3

howManyAboveAverage :: Integer -> Integer -> Integer -> Integer
howManyAboveAverage a b c = toInteger $ length $ filter (>avg) [fromInteger a, fromInteger b, fromInteger c]
  where avg = averageThree a b c


testAvg1 = TestCase(assertEqual "for howManyAboveAverage 1 1 1" 0 (howManyAboveAverage 1 1 1))
testAvg2 = TestCase(assertEqual "for howManyAboveAverage 1 1 2" 1 (howManyAboveAverage 1 1 2))
testAvg3 = TestCase(assertEqual "for howManyAboveAverage 1 3 3" 2 (howManyAboveAverage 1 2 1))
testAvg4 = TestCase(assertEqual "for howManyAboveAverage 5 6 1" 2 (howManyAboveAverage 2 1 1))
testAvg5 = TestCase(assertEqual "for howManyAboveAverage 100 2 2" 1 (howManyAboveAverage 1 3 2))

testsAvg = TestList [testAvg1, testAvg2, testAvg3, testAvg4, testAvg5]


powerTwo :: Integer -> Integer
powerTwo n
  | n <= 0 = 1
  | mod n 2 == 0 = powerTwo (toInteger (div n 2)) ^ 2
  | otherwise = powerTwo (toInteger (div (n-1) 2)) ^ 2 * 2

testPower1 = TestCase(assertEqual "for powerTwo 3" 8 (powerTwo 3))
testPower2 = TestCase(assertEqual "for powerTwo 6" 64 (powerTwo 6))
testPower3 = TestCase(assertEqual "for powerTwo 0" 1 (powerTwo 0))
testPower4 = TestCase(assertEqual "for powerTwo 18" 262144 (powerTwo 18))
testPower5 = TestCase(assertEqual "for powerTwo -1" 1 (powerTwo (-1)))

testsPower = TestList [testPower1, testPower2, testPower3, testPower4, testPower5]