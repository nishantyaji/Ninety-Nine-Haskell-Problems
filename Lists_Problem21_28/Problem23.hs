-- Problem 23
-- Extract a given number of randomly selected elements from a list.

-- note the usage of randomRIO

import System.Random (randomRIO)

-- Function to generate a random number between 1 and a given upper limit
generateRandomNumber :: Int -> IO Int
generateRandomNumber l = randomRIO (1, l)

-- Function to generate a list of 'n' random numbers between 1 and a given upper limit
getRand :: Int -> Int -> IO [Int]
getRand 0 _ = return []
getRand n l = do
  num <- generateRandomNumber l
  nums <- getRand (n - 1) l
  return (num : nums)

-- Function to get the character at the given index in a list
charAt :: (Eq a) => [a] -> Int -> a
charAt [] _ = error "Index out of bounds"
charAt [x] 1 = x
charAt (x : xs) 1 = x
charAt (x : xs) n = charAt xs (n - 1)

-- Function to get characters at multiple indices in a list
charAtMany :: (Eq a) => [a] -> [Int] -> [a]
charAtMany _ [] = []
charAtMany xs (r : rands) = (charAt xs r) : charAtMany xs rands

-- Function to select 'n' random elements from a list
-- do block because it uses IO/monad
rnd_select :: (Eq a) => [a] -> Int -> IO [a]
rnd_select xs n = do
  indices <- getRand n (length xs)
  return (charAtMany xs indices)

-- Example usage
main :: IO ()
main = do
  result <- rnd_select "abcdefghik" 3
  putStrLn $ "Randomly selected elements: " ++ show result
