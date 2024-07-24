-- Problem 21
-- Insert an element at a given position into a list.

insertAt :: (Eq a) => a -> [a] -> Int -> [a]
insertAt x xs n = take (n - 1) xs ++ [x] ++ drop (n - 1) xs

-- Usage
-- insertAt 'X' "abcd" 2
-- "aXbcd"