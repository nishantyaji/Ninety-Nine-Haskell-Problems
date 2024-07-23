-- Problem 14
-- (*) Duplicate the elements of a list.

repeat' :: Int -> a -> [a]
repeat' 0 _ = []
repeat' 1 x = [x]
repeat' n x = x : repeat' (n - 1) x

dupli :: (Eq a) => [a] -> [a]
dupli [] = []
dupli (x : xs) = repeat' 2 x ++ dupli xs

-- dupli (x : xs) = x : x : dupli xs

-- Usage
-- dupli [1,2,3]
-- output
-- [1,1,2,2,3,3]