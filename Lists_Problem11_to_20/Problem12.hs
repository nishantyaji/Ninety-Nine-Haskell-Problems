-- Problem 12
-- (**) Decode a run-length encoded list.

-- new data structure
data Encoded a = Single a | Multiple Int a deriving (Show)

repeat' :: Int -> a -> [a]
repeat' 0 _ = []
repeat' 1 x = [x]
repeat' n x = x : repeat' (n - 1) x

decodeModified :: [Encoded a] -> [a]
decodeModified [] = []
decodeModified [Single x] = [x]
decodeModified [Multiple n x] = repeat' n x
decodeModified (x : xs) = decodeModified [x] ++ decodeModified xs

-- Usage
-- decodeModified [Multiple 4 'a',Single 'b',Multiple 2 'c', Multiple 2 'a',Single 'd',Multiple 4 'e']
-- output: aaaabccaadeeee