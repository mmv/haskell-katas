-- solutions for Euler Project problems
--
import Data.Char

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
    maximum (filter isPalindrome products)
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

-- problem 7
-- 10 001 st prime

primes =
    2 : nextPrimes 3
    where
        isPrime n = all (\x -> (mod n x) > 0) (takeWhile (< (div n 2)) primes)
        nextPrimes n =
            if isPrime n
            then n : nextPrimes (n + 2)
            else     nextPrimes (n + 2)

problem7 () = primes !! 10000 -- 0-based, so we're getting the 10001

-- problem 8
-- greatest product for 5 consecutive digits


greatestProductOfXConsecutive numdigits digits =
    foldl max 0 (map (foldl (*) 1) (splitSeq digits numdigits))
    where
        splitSeq [] len = []
        splitSeq sx len =
            (take len sx) : splitSeq (tail sx) len

problem8 () = do
    contents <- readFile "inputs/problem8.txt"
    let fullstr = (concat (lines contents))
    let asdigits = (map digitToInt fullstr)
    return (greatestProductOfXConsecutive 5 asdigits)

-- problem 9
-- pitagorian triplet (a^2+b^2=c^2) where a+b+c == 1000

tripletsEqualTo n =
    filter (\(a,b,c) -> a*a + b*b == c*c) (iter (1,2,n-3))
    where
        iter (a,b,c) | a < 0 || b < 0 || c < 0 = []
                     | b >= c = iter (a+1,b-1,c)
                     | a >= b = iter (1,n-(c-1)-1,c-1)
                     | a <  b = (a,b,c) : iter (a+1,b-1,c)
                     | otherwise = [(0,0,0)]

problem9 () = foldl max 0 $ map (\(a,b,c) -> a*b*c) $ tripletsEqualTo 1000
