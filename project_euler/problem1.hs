import Data.List (union)
problem_1' = sum (union [3,6..999] [5,10..999])
 
problem_1  = sum [x | x <- [1..999], x `mod` 3 == 0 || x `mod` 5 == 0]