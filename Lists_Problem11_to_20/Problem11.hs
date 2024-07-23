-- Problem 11
-- (*) Modified run-length encoding.

-- new data structure
data Encoded a = Single a | Multiple Int a deriving (Show)

-- deriving show enables the data to be printed

encodeModified :: (Eq a) => [a] -> [Encoded a]
encodeModified [] = []
encodeModified [x] = [Single x]
encodeModified [x, y]
  | x == y = [Multiple 2 x]
  | otherwise = [Single x, Single y]
encodeModified (x : y : xs)
  | x == y = (Multiple (1 + length (takeWhile (== x) (y : xs))) x) : encodeModified (dropWhile (== x) (y : xs))
  | otherwise = (Single x) : encodeModified (y : xs)

-- Usage
-- encodeModified "aaaabccaadeeee"
-- outputs
-- [Multiple 4 'a',Single 'b',Multiple 2 'c', Multiple 2 'a',Single 'd',Multiple 4 'e']