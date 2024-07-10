-- Problem 5
-- (*) Reverse a list.

myReverse :: [a] -> [a]
myReverse [] = []
myReverse [x] = [x]
myReverse [x, y] = [y, x]
myReverse (x : xs) = myReverse xs ++ [x]

-- Note that ++ is the list concatenation operator

-- Usage
-- myReverse [1,2,3,4]
-- myReverse "Hello World! Haskell"