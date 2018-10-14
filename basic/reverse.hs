a_reverse :: [Integer] -> [Integer]

a_reverse [] = []
a_reverse (h:t) = a_reverse t ++ [h]
