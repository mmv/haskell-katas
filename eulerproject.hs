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

