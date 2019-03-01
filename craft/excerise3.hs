module Cap where

cap n
  | ('a' <= n)&&(n <= 'z') = toEnum (fromEnum n - 32)
  | otherwise = n

charToNum :: Char -> Int
charToNum c
  | ('0' <= c)&&(c <= '9') = fromEnum c - 48
  | otherwise = 0

onThreeLines :: String -> String -> String -> String
onThreeLines a b c = a++"\n"++b++"\n"++c

romanDigit :: Char -> String
romanDigit d
  | d == '0' = "O"
  | d == '1' = "I"
  | d == '2' = "II"
  | d == '3' = "III"
  | d == '4' = "IV"
  | d == '5' = "V"
  | d == '6' = "VI"
  | d == '7' = "VII"
  | d == '8' = "VIII"
  | d == '9' = "IX"
  | otherwise = "O"

averageThree :: Integer -> Integer -> Integer -> Float
averageThree a b c = fromIntegral(a+b+c)/3

howManyAboveAverage :: Integer -> Integer -> Integer -> Integer
howManyAboveAverage a b c = toInteger (length (filter (>avg) (map (fromIntegral) [a,b,c])))
    where avg = (averageThree a b c) 

numberNDroots :: Float -> Float -> Float -> Float
numberNDroots a b c
  | b*b > 4.0*a*c = 2
  | b*b == 4.0*a*c = 1
  | otherwise = 0

numberRoots :: Float -> Float -> Float -> Float
numberRoots a b c
  | a /= 0 = (numberNDroots a b c)
  | b /= 0 = 1
  | c /= 0 = 0
  | otherwise = 3

smallerRoot :: Float -> Float -> Float -> Float
smallerRoot a b c
  | n == 0 || n == 3 = 0
  | otherwise = ((-1)*b-(sqrt (b*b-4.0*a*c)))/(2.0*a)
  where n = (numberRoots a b c)

largerRoot :: Float -> Float -> Float -> Float
largerRoot a b c
  | n == 0 || n == 3 = 0
  | otherwise = ((-1)*b+(sqrt (b*b-4.0*a*c)))/(2.0*a)
  where n = (numberRoots a b c)

funny x = x+x
peculiar y = y
