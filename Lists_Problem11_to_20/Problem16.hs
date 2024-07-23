-- Problem 16
-- (**) Drop every N'th element from a list.

drop' :: (Eq a) => [a] -> Int -> Int -> [a]
drop' [] _ _ = []
drop' (x : xs) p q
  | mod p q == 0 = drop' xs (p + 1) q
  | otherwise = x : drop' xs (p + 1) q

dropEvery :: (Eq a) => [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery (x : xs) n = drop' (x : xs) 1 n

-- Usage
-- dropEvery [1,2,3,4,5,6,7,8,9,10] 3
-- output
-- [1,2,4,5,7,8,10]
--
-- drop "abcdefghijk" 3
-- "abdeghjk"