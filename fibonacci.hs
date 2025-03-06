--problem 2

fibbUpTo :: Int -> [Int]
fibbUpTo m = fibHelper 0 1 m

fibHelper :: Int -> Int -> Int -> [Int]
fibHelper a b m
    | a > m = []
    | otherwise = a : fibHelper b (a+b) m

summ :: Int -> Int
summ n = sum( filter even (fibbUpTo n))


main :: IO()
main = print (summ 4000000)