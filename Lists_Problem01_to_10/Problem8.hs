-- Problem 8
-- (**) Eliminate consecutive duplicates of list elements.
compress :: (Eq a) => [a] -> [a]
compress [] = []
compress [x] = [x]
compress [x, y]
  | x == y = [x]
  | otherwise = [x, y]
compress (x : y : xs)
  | x == y = compress (y : xs)
  | otherwise = [x] ++ compress (y : xs)

-- Usage
-- compress "aaaabccaadeeee"
-- compress [1,1,2,3,4,4,4,5,5,5,5,6,7,8,9,9,9]
