-- Problem 4
-- (*) Find the number of elements in a list.

myLength :: [a] -> Int
myLength [] = 0
myLength [x] = 1
myLength (x : xs) = 1 + myLength xs

-- usage
-- myLength [1,2,3,4]
-- myLength "Hello World! Haskell"