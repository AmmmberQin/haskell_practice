size [] = 0
size (h:t) = 1 + size t

prod [] = 0
prod (h:t) = h + prod t
