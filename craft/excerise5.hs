import Test.HUnit
import Data.List 
import Data.Char
import Data.String

maxOccurs :: Integer -> Integer -> (Integer, Integer)
maxOccurs a b
  | a==b = (a, 2)
  | otherwise = ((max a b), 1)

testMax1 = TestCase(assertEqual "for maxOccurs 1 1" (1, 2) (maxOccurs 1 1))
testMax2 = TestCase(assertEqual "for maxOccurs 1 2" (2, 1) (maxOccurs 2 1))

testsMax = TestList [testMax1, testMax2]

maxThreeOccurs :: Integer -> Integer -> Integer -> (Integer, Integer)
maxThreeOccurs a b c
  | j == 2 = (n, (j+i-1))
  | j == 1 = (n, 1)
  where (m, i) = maxOccurs a b
        (n, j) = maxOccurs m c

testMaxThree1 = TestCase(assertEqual "for maxThreeOccurs 1 3 2" (3, 1) (maxThreeOccurs 1 3 2))
testMaxThree2 = TestCase(assertEqual "for maxThreeOccurs 1 1 3" (3, 1) (maxThreeOccurs 1 1 3))
testMaxThree3 = TestCase(assertEqual "for maxThreeOccurs 1 3 3" (3, 2) (maxThreeOccurs 1 3 3))
testMaxThree4 = TestCase(assertEqual "for maxThreeOccurs 3 3 1" (3, 2) (maxThreeOccurs 3 3 1))
testMaxThree5 = TestCase(assertEqual "for maxThreeOccurs 3 1 3" (3, 2) (maxThreeOccurs 3 1 3))
testMaxThree6 = TestCase(assertEqual "for maxThreeOccurs 3 3 3" (3, 3) (maxThreeOccurs 3 3 3))
testMaxThree7 = TestCase(assertEqual "for maxThreeOccurs 3 1 1" (3, 1) (maxThreeOccurs 3 3 1))

testsMaxThree = TestList [testMaxThree1, testMaxThree2, testMaxThree3, testMaxThree4, 
                          testMaxThree5, testMaxThree6, testMaxThree7]

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

testOrder1 = TestCase(assertEqual "for orderTriple (1,2,3)" (1,2,3) (orderTriple (1,2,3)))
testOrder2 = TestCase(assertEqual "for orderTriple (1,3,2)" (1,2,3) (orderTriple (1,3,2)))
testOrder3 = TestCase(assertEqual "for orderTriple (3,2,1)" (1,2,3) (orderTriple (3,2,1)))
testOrder4 = TestCase(assertEqual "for orderTriple (3,1,2)" (1,2,3) (orderTriple (3,1,2)))

testsOrder = TestList [testOrder1, testOrder2, testOrder3, testOrder4]

accrossX :: (Float, Float) -> Float
accrossX (a, b)
  | a == 0 = 0
  | otherwise = (-1)*b / a 


data People = People Name Age deriving (Eq, Show)
type Name = String
type Age = Int

showPerson :: People -> String
showPerson (People st n) = st ++ "--" ++ show n

type Center = (Float, Float)

data Shape = Circle Float Center|
             Rectangle Float Float Center|
             Triangle Float Float Float Center
             deriving (Eq, Ord, Show)

isRound :: Shape -> Bool
isRound (Circle _ _) = True
isRound (Rectangle _ _ _) = False
isRound (Triangle _ _ _ _) = False

isRegular :: Shape -> Bool
isRegular (Circle _ _) = True
isRegular (Rectangle h w _) = h==w
isRegular (Triangle a b c _) = a==b && b==c

isEqual :: Shape -> Shape -> Bool
isEqual (Circle a _) (Circle b _)
  | a>=0 && b>=0 = a==b
  | a<0 && b<0 = True
  | otherwise = False

isEqual (Rectangle a b _) (Rectangle c d _)
  | a>=0 && b>=0 && c>=0 && d>=0 = (a==c&&b==d)||(a==d&&b==c)
  | a<0 && b<0 && c<0 && d<0 = True
  | otherwise = False

isEqual (Triangle a b c _) (Triangle d e f _)
  | i>=0 && m>=0 = i==m&&j==n&&k==o
  | k<0 && o<0 = True
  | otherwise = False
  where [i,j,k] = sort [a,b,c]
        [m,n,o] = sort [d,e,f]

area :: Shape -> Float
area (Circle r _) = pi*r*r
area (Rectangle h w _) = h * w
area (Triangle a b c r) = sqrt (p*(p-a)*(p-b)*(p-c))
    where p = perimeter (Triangle a b c r) / 2

perimeter :: Shape -> Float
perimeter (Circle r _) = 2*pi*r
perimeter (Rectangle h w _) = 2*(h+w)
perimeter (Triangle a b c _) = a+b+c

data ShopItem = ShopItem String Int deriving (Eq, Show)

move :: Float -> Float -> Shape -> Shape
move x y (Circle r (i,j)) = Circle r (x+i, y+j)

isOverlap :: Shape -> Shape -> Bool
isOverlap (Circle r1 (x1, y1)) (Circle r2 (x2, y2)) = sqrt ((x1-x2)^2+(y1-y2)^2) < (r1+r2)
isOverlap (Rectangle h1 w1 (x1,y1)) (Rectangle h2 w2 (x2,y2)) = x<h && y<w
  where x = abs (x1-x2)
        y = abs (y1-y2)
        h = abs (h1+h2)/2
        w = abs (w1+w2)/2
-- isOverlap (Triangle a1 b1 c1 (x1,y1)) (Triangle a2 b2 c2 (x2,y2)) 
-- idk

data Address = HouseName String | HouseNumber Integer deriving (Eq, Show)

showAddress :: Address -> String
showAddress (HouseName name) = name
showAddress (HouseNumber num) = show num

doubleAll :: [Integer] -> [Integer]
doubleAll ex = [e*2 | e <- ex]

capitalize :: String -> String
capitalize cs = [if (isAlpha c) then (toUpper c) else c | c <- cs] 

capitalizeLetters :: String -> String
capitalizeLetters cs = [toUpper c | c <- cs, isAlpha c]

divisors :: Integer -> [Integer]
divisors n = [i | i <- [1..n], (mod n i) == 0]

isPrime :: Integer -> Bool
isPrime n = [1,n] == (divisors n)

matches :: Integer -> [Integer] -> [Integer]
matches n m = [i | i<-m, i==n]

ielem :: Integer -> [Integer] -> Bool
ielem n m = [] /= (matches n m)

onSeparateLines :: [String] -> String
onSeparateLines s
  | tail s == [] = head s
  | otherwise = head s ++ "\n" ++ (onSeparateLines (tail s))

duplicate :: String -> Integer -> String
duplicate s n
  | n <= 0 = ""
  | n == 1 = s
  | otherwise = s ++ (duplicate s (n-1))

pushRight :: String -> Integer -> String
pushRight s n
  | (toInteger (length s))>=n = drop ((length s)-(fromIntegral n)) s
  | otherwise = " " ++ (pushRight s (n-1))

fib n
  | n == 0 = 0
  | n == 1 = 1
  | otherwise = (fib (n-1)) + (fib (n-2))

fibTable :: Integer -> String
fibTable n
  | n < 0 = "n" ++ (pushRight "fib n" 16) ++ "\n"
  | otherwise = (fibTable (n-1)) ++ (show n) ++ (pushRight (show (fib n)) 16) ++ "\n"
