import Data.List

input :: [Int]
input = [14,0,15,12,11,11,3,5,1,6,8,4,9,1,8,4]

reallocateUntilSeenBefore :: [[Int]] -> [Int]-> Int
reallocateUntilSeenBefore seen blocks = case reallocatedBlock `elem` seen  of
                            True -> length seen + 1
                            False -> reallocateUntilSeenBefore (reallocatedBlock:seen) reallocatedBlock
                        where reallocatedBlock = reallocate blocks

reallocate :: [Int] -> [Int]
reallocate xs = (\(x,y,z) -> z) $ redistribute $ extractLargestElement xs

redistribute :: (Int, Int, [Int]) -> (Int, Int, [Int])
redistribute (0, x, xs) = (0, x, xs)
redistribute x = redistribute $ distributeToNextBlock x                              

distributeToNextBlock :: (Int, Int, [Int]) -> (Int, Int, [Int])
distributeToNextBlock (numberLeft, index, xs) = (numberLeft-1, currentIndex, incrementElement xs currentIndex)
                                            where currentIndex = ((index+1) `mod` (length xs))

incrementElement ::  [Int] -> Int -> [Int]
incrementElement xs index = (take index xs) ++ [((xs !! index) + 1)] ++ drop (index+1 ) xs

indexOfMaxElement :: [Int] -> Int
indexOfMaxElement xs = case elemIndex (maximum xs) xs 
                       of 
                        Nothing -> 0
                        Just x -> x

extractLargestElement :: [Int] -> (Int, Int, [Int])
extractLargestElement xs = (maximum xs, indexOfMaxElement xs, remainingElems)
                        where remainingElems = firstPart ++ [0] ++ (drop (length firstPart + 1) xs)
                              firstPart = (takeWhile (\x -> x /= maximum xs) xs)