-- Problem 19
-- (**) Rotate a list N places to the left.

rotate::Eq a => [a] -> Int -> [a]
rotate [] _ = []
rotate [x] _ = [x]
rotate xs n 
 | n == 0 = xs
 | n > 0 = drop n xs ++ take n xs 
 | n < 0 = drop ((length xs) + n) xs ++ take ((length xs) + n) xs

 -- Usage
 -- rotate "abcdefghi" 3
 -- output
 -- "defghiabc"
 -- rotate "abcdefghi" (-2)
 -- "hiabcdefg"