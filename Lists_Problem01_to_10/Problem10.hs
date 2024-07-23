-- Problem 10
-- (*) Run-length encoding of a list.

encode :: (Eq a) => [a] -> [(Int, a)]
encode [] = []
encode [a] = [(1, a)]
encode (x : xs) = (1 + length (takeWhile (== x) xs), x) : encode (dropWhile (== x) xs)

-- Usage
-- encode "aaaabccaadeeee"
-- returns
-- [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]