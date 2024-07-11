-- Problem 6
-- (*) Find out whether a list is a palindrome.

isPalindrome:: Eq a => [a] -> Bool
isPalindrome [] = True
isPalindrome [_] = True
isPalindrome (x : xs) = (x == last xs) && isPalindrome (init xs)

-- note the Eq a constraint meant for "==" operator


-- isPalindrome word = word == reverse word
-- Usage
-- isPalindrome "Malayalam"
-- isPalindrome [1,2,1]