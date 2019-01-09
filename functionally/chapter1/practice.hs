-- Practice A
double :: Integer -> Integer
double x = 2*x

-- map double [1,4,4,3]
-- map (double . double) [1,4,4,3]

-- Practice B
-- sin (2*theta) / (2*pi)

-- Practice G
num1 = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
num2 = ["One", "Two", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine"]
line2 = "Went to mow a meadow\n"
line1 n
  |n==1 = "One man went to mow\n"
  |otherwise = (num2!!(n-1)) ++ " men went to mow\n"
counting n
  |n==0 = ""
  |n==1 = "one man"
  |otherwise = (num1!!(n-1)) ++ " men, " ++ (counting (n-1))

line3 n
  |n==1 = "One man and his dog\n"
  |otherwise = (num2!!(n-1)) ++ " men, " ++ (counting (n-1)) ++ " and his dog\n"

song n = (line1 n) ++ line2 ++ (line3 n) ++ line2