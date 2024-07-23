-- Problem 7
-- (**) Flatten a nested list structure.

-- create a data structure (?), which can be a list or an element)
data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem a) = [a]
flatten (List xs) = concatMap flatten xs

-- above is same as
-- concat $ map flatten xs

-- flatten xs returns [[x]]
-- concatMap then does [x .....]

-- copied from https://www.reddit.com/r/haskellquestions/comments/3han54/how_do_you_flatten_a_nested_list_of_arbitrary/

-- usage flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])