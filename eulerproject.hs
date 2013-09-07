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

-- problem 3:
-- largest prime factor of ...

largestPrimeOf n =
    factor n 2
    where
        divisibleBy x y = (mod x y) == 0
        factor n k =
            if k > (div n 2)
            then n
            else
                if divisibleBy n k
                then factor (div n k) k
                else factor n (k+1)

problem3 () = largestPrimeOf 600851475143

-- problem 4
-- largest palindrome (same read both ways)
largestPalindrome (products) =
    maximum (filter isPalindrome sx)
    where
        decompose n =
            let (q,r) = divMod n 10
            in
                r : if q == 0 then [] else decompose q
        isPalindrome n =
            (decompose n) == (reverse (decompose n))
            where

-- we want the largest palindrome created by product of two 3-digit ints
problem4 () = largestPalindrome [ x*y | x <- [ 100..999 ], y <- [ 100..999 ], x >= y ]

-- problem 5
-- smallest number evenly divisible for all integers up to X
smallestDivisibleForAllUpTo n =
    product (primify [ 2..n ])
    where
        divisibleBy x y = (mod x y) == 0
        primify [] = []
        primify (x:xs) = x : primify (map
                                        (\a -> if (divisibleBy a x)
                                               then (div a x)
                                               else a)
                                        xs)

problem5 () = smallestDivisibleForAllUpTo 20

-- problem 6
-- difference between sum of squares and square of sums
problem6 () = sum([1..100])^2 - sum([ x*x | x <- [1..100] ])

