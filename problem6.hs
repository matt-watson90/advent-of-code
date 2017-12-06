import Data.List

input :: [Int]
input = [14,0,15,12,11,11,3,5,1,6,8,4,9,1,8,4]

part1 :: Int
part1 =   fst $ reallocateUntilSeenBefore [] input

part2 :: Int
part2 =  (\x -> x-1) $ fst $ reallocateUntilSeenBefore [] (snd $ (reallocateUntilSeenBefore [] input))

reallocateUntilSeenBefore :: [[Int]] -> [Int]-> (Int, [Int])
reallocateUntilSeenBefore seen blocks = case reallocatedBlock `elem` seen  of
                            True -> (length seen + 1, reallocatedBlock)
                            False -> reallocateUntilSeenBefore (reallocatedBlock:seen) reallocatedBlock
                        where reallocatedBlock = reallocate blocks

reallocate :: [Int] -> [Int]
reallocate xs = (\(x,y,z) -> z) $ distributeLargestBlock $ extractLargestElement xs

distributeLargestBlock :: (Int, Int, [Int]) -> (Int, Int, [Int])
distributeLargestBlock (0, x, xs) = (0, x, xs) -- We've reallocated the largest block
distributeLargestBlock x = distributeLargestBlock $ distributeToNextBlock x -- Distribute one of whatever we've got left to the next block 
                                                                            -- and then keep going                              

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
