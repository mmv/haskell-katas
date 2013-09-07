-- solutions for Euler Project problems
--

-- problem 1:
-- multiples of 3 or 5
sumMultiplesOf3or5Below n =
    sum (filter isMultiple [ 1..(n-1) ])
    where
        isMultiple n =
            (mod n 3) == 0 ||
            (mod n 5) == 0

problem1 () = sumMultiplesOf3or5Below 1000

-- problem 2:
-- sum of fib even numbers below 4M

sumFibEvensBelow n =
    sum (takeWhile
            (\x -> x < n)
            (filter (\x -> even x) fibs))
    where
        fibseq p1 p2 = p1 : fibseq p2 (p1+p2)
        fibs = fibseq 1 2

problem2 () = sumFibEvensBelow 4000000


