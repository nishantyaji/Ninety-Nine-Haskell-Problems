-- Problem 18
-- (**) Extract a slice from a list.

choose' ::[a] -> Int -> Int -> Int -> [a]
choose' [] _ _ _ = []
choose' (x:xs) p q n
  | p <= n && q >= n = x : choose' xs p q (n+1)
  | otherwise = choose' xs p q (n+1)

slice::[a] -> Int -> Int -> [a]
slice [] _ _ = []
slice x p q = choose' x p q 1

-- Usage
-- slice "abcdefghijk" 3 7 
-- output
-- "cdefg"