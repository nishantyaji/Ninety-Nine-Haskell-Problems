-- Problem 2
-- (*) Find the last-but-one (or second-last) element of a list.

myLast :: [a] -> a
myLast [x] = x
myLast (x:xs) = myLast xs

myButLast :: [a] -> a
myButLast a = myLast (init a)


-- Usage
-- myButLast [1,2,3,4]
-- myButLast ['a', 'b', 'c', 'd']\

-- Please refer this link for solutions from other folks:
-- https://github.com/search?q=myButLast++language%3AHaskell&type=code&l=Haskell