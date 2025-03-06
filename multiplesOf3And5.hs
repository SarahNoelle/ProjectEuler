--Problem 1 of ProjectEuler

findMultiplesOf3And5 :: Int -> [Int]
findMultiplesOf3And5 0 = []
findMultiplesOf3And5 n = [x | x <- [1..n-1], x `mod` 3 == 0 || x `mod` 5 == 0]

summ :: Int -> Int
summ n = sum(findMultiplesOf3And5 n)


main :: IO()
main = print (summ 1000)




