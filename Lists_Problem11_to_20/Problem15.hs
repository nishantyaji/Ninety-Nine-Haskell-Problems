-- Problem 15
-- (**) Replicate the elements of a list a given number of times.

repeat' :: Int -> a -> [a]
repeat' 0 _ = []
repeat' 1 x = [x]
repeat' n x = x : repeat' (n - 1) x

repli :: (Eq a) => [a] -> Int -> [a]
repli [] _ = []
repli (x : xs) n = repeat' n x ++ repli xs n

-- Usage
-- repli "abc" 3
-- "aaabbbccc"