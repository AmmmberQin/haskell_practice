allEven :: [Integer] -> [Integer]
allEven x = [ a | a<-x, mod a 2 == 0]
