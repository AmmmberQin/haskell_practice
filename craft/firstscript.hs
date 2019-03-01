module FirstScript where

size :: Integer
size = 12+13

square :: Integer -> Integer
square n = n*n

double :: Integer -> Integer
double n = 2*n

example :: Integer
example = double (size - square (2+2))

extra_1 :: Integer -> Integer
extra_1 = square.double

extra_2 :: Integer -> Integer
extra_2 = double.square
