-- @Author: Li Qin
-- @Date:   2019-04-11 15:21:06
-- @Last Modified by:   Li Qin
-- @Last Modified time: 2019-04-11 15:31:43
import Data.Map 
import Prelude hiding (map, lookup)
sequenceElement a n = elements a n (fromList [])

elements a n d
  | n < length a = a!!n
  | j == -1 = elements (a++[i]) n (insert s m d)
  | otherwise = a!!(j + (mod (n-m) (m-j)))
  where index k = element (lookup k d)
        s = show (drop (m-5) a)
        i = mod (sum$drop (m-5) a) 10
        m = length a
        j = index s

element (Just a) = a
element Nothing = -1

-- # PYTHON
-- def sequenceElement(a, n):
--     dic = {}
--     while True:
--         if n < len(a):
--             return a[n]
        
--         last_5_items = a[-5:]
--         str_last_5_items = " ".join([str(i) for i in last_5_items])
--         if str_last_5_items in dic:
--             index = dic[str_last_5_items]
--             return a[index+((n-len(a))%(len(a)-index))]
--         dic[str_last_5_items] = len(a)
--         next_item = sum(last_5_items) % 10
--         a.append(next_item)