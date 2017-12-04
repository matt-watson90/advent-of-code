import Data.Char

solve :: Integer -> Int
solve x = doTheCalculation $ listOfInt
    where listOfInt = map digitToInt $ show x

doTheCalculation :: [Int] -> Int
doTheCalculation (x:xs) 
                | firstAndLastAreTheSame = x + totalForConsecutiveNumbers(x:xs)
                | otherwise =totalForConsecutiveNumbers (x:xs)
            where firstAndLastAreTheSame = x == head (reverse xs)

totalForConsecutiveNumbers :: [Int]-> Int
totalForConsecutiveNumbers [] = 0
totalForConsecutiveNumbers (x:xs) = areEqual x xs + totalForConsecutiveNumbers xs 

areEqual :: Int -> [Int] -> Int
areEqual _ [] = 0
areEqual a bs 
            | (a == head bs) = a
            | otherwise = 0 