-- Problem 3
-- (*) Find the K'th element of a list.

elementAt :: [a] -> Int -> a
elementAt (x : xs) k
  | k == 1 = x
  | otherwise = elementAt xs (k - 1)

elementAt' :: [a] -> Int -> a
elementAt' (x : xs) 1 = x
elementAt' (x : xs) k = elementAt' xs (k - 1)

-- usage
-- elementAt' 2 [1,2,3,4,5]