-- Problem 13
-- (**) Run-length encoding of a list (direct solution).

-- new data structure
data Encoded a = Single a | Multiple Int a deriving (Show)

encodeDirect :: (Eq a) => [a] -> [Encoded a]
encodeDirect [] = []
encodeDirect [x] = [Single x]
encodeDirect [x, y]
  | x == y = [Multiple 2 x]
  | otherwise = [Single x, Single y]
encodeDirect (x : y : xs)
  | x == y = (Multiple (1 + length (takeWhile (== x) (y : xs))) x) : encodeDirect (dropWhile (== x) (y : xs))
  | otherwise = (Single x) : encodeDirect (y : xs)

-- See Problem 11