import Data.Char


solvePart1 :: Integer -> Int
solvePart1 x = doTheCalculation (listOfInt x) 1

solvePart2 :: Integer -> Int
solvePart2 x = doTheCalculation list gap
    where list = (listOfInt x)  
          gap = (length list) `div` 2

listOfInt :: Integer -> [Int]
listOfInt x =  map digitToInt $ show x

doTheCalculation :: [Int] -> Int-> Int
doTheCalculation xs gap = totalForGapN circularList gap
                            where circularList = xs ++ (take gap xs)
            

totalForGapN :: [Int] -> Int -> Int
totalForGapN [] gap = 0
totalForGapN (x:xs) gap = currentElement + totalForGapN xs gap
                        where currentElement = areEqual gap (x:xs)

areEqual :: Int -> [Int] -> Int
areEqual _ [] = 0
areEqual gap (b:bs) 
            | length bs < gap = 0
            | (b == (b:bs)!!gap) = b
            | otherwise = 0 
