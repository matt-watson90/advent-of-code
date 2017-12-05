import Data.List
import Data.List.Split

main ::  IO()
main = do
  filecontent <- readFile "problem4-input.txt"
  putStrLn (show $ length $ filter containsNoDuplicates $ convertToIndividualWords $ lines filecontent)

main2 ::  IO()
main2 = do
  filecontent <- readFile "problem4-input.txt"
  putStrLn (show $ length $ filter containsNoDuplicates $ map sortEachString $ convertToIndividualWords $ lines filecontent)

  convertToIndividualWords :: [String] -> [[String]]
  convertToIndividualWords xs = map (splitOn " ") xs
  
  containsNoDuplicates :: [String] -> Bool
  containsNoDuplicates xs = xs == nub xs
  
  sortEachString :: [String] -> [String]
  sortEachString xs = map sort xs
  