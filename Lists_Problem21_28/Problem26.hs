-- Problem 26
-- (**) Generate combinations of K distinct objects chosen from the N elements of a list.

-- combinations 3 "abcdef"
-- ["abc","bcd","cde","def".. ]

combinations :: (Eq a) => Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations k (x : xs) = (map (x :) (combinations (k - 1) xs)) ++ combinations k xs

{-
Combination k-1 xs creates all the combination of size k-1 without having the 1st element i.e. x
map(x:) combinations (k-1) xs adds the 1st element i.e. x to all the combinations of size k - 1)
combination k xs: takes care of all combinations of size k but without the element x in it

-}