-- Problem 25
-- Generate a random permutation of the elements of a list.

import System.Random (randomRIO)

-- Function to generate a random number between 1 and a given upper limit
-- Note the left arrow (<-) symbol for IO based input
-- Note the if then else statement that seems to handle the <- case
generateRandomNumber :: Int -> [Int] -> IO Int
generateRandomNumber l ls = do
  num <- randomRIO (1, l)
  if num `elem` ls then generateRandomNumber l ls else return num

-- Function to generate a list of 'n' random numbers between 1 and a given upper limit
getRand :: Int -> Int -> IO [Int]
getRand 0 _ = return []
getRand m n = do
  nums <- getRand (m - 1) n
  num <- generateRandomNumber n nums
  return (num : nums)

-- Function to get the character at the given index in a list
charAt :: (Eq a) => [a] -> Int -> a
charAt [] _ = error "Index out of bounds"
charAt (x : _) 1 = x
charAt (_ : xs) n = charAt xs (n - 1)

-- Function to get characters at multiple indices in a list
charAtMany :: (Eq a) => [a] -> [Int] -> [a]
charAtMany _ [] = []
charAtMany xs (r : rands) = (charAt xs r) : charAtMany xs rands

rnd_permu :: (Eq a) => [a] -> IO [a]
rnd_permu xs = do
  indices <- getRand (length xs) (length xs)
  return (charAtMany xs indices)

-- Usage
-- rnd_permu "abcdefg"
-- Output
-- "ebdfgca"