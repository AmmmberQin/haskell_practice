getKey key [] = Nothing
getKey key ((k,v):xs) = if (key == k) then return v else getKey key xs
