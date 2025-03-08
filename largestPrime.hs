--Problem 3

-- Method: sieve of Eratosthenes
primes::[Integer]
primes = helper [2..775146]
    where
        helper (z:xs) = z: helper[x|x <- xs, x `mod` z /= 0]


primFactor :: [Integer] -> Integer -> [Integer]
primFactor [] _  = []
primFactor (p:ps) n
    | p*p > n = if n > 1 then [n] else []
    | n `mod` p == 0 = p: primFactor (p:ps) (n `div` p) 
    |otherwise =  primFactor ps n 



maxx :: [Integer] -> Integer
maxx xs = maximum xs 






main :: IO()
main = do
    let number = 600851475143
        factors = primFactor primes number  
        largestFactor = maxx factors         
    print largestFactor