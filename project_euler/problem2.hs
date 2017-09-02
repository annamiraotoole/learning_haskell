problem_2 = sumEvenFibsLessThan 1000000
 
sumEvenFibsLessThan n = (a + b - 1) `div` 2
  where
    n2 = n `div` 2
    (a, b) = foldr f (0,1)
             . takeWhile ((<= n2) . fst)
             . iterate times2E $ (1, 4)
    f x y | fst z <= n2 = z
          | otherwise   = y
      where z = x `addE` y
addE (a, b) (c, d) = (a*d + b*c - 4*ac, ac + b*d)
  where ac=a*c
 
times2E (a, b) = addE (a, b) (a, b)