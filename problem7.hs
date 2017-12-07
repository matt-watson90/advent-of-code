import Text.Regex
import Data.List
import Data.List.Unique

main ::  IO()
main = do
  filecontent <- readFile "problem7-input.txt"
  putStrLn $ show $ unique $ flattenPrograms $ map (filter isAWord) $ map (splitRegex  matchSpacesAndCommas) (lines filecontent)

flattenPrograms :: [[String]] -> [String]
flattenPrograms [] = []
flattenPrograms (x:xs) = x  ++ flattenPrograms xs 

isAWord :: String -> Bool
isAWord x = matchRegex matchOnlyWords x /= Nothing

matchSpacesAndCommas :: Regex
matchSpacesAndCommas = mkRegex "\\s|," 

matchOnlyWords :: Regex
matchOnlyWords = mkRegex "[a-z]+" 