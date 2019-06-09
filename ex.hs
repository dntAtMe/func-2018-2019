import Data.List

first [] _ = []
first xs k 
        | length xs == k     = [xs]
        | length xs < k      = []
first ns@(n1:ns') k = helper ns k 
    where
        helper (x:[]) _ = []
        helper ns@(n1:ns') 2 = take 2 ns : helper (n1:drop 1 ns') 2

