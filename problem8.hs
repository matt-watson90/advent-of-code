import qualified Data.Text as T
import Data.List

type ID = String
type Value = Int

data Register = Register ID Value deriving (Show, Eq)
type Registers = [Register]

data Modifier = Inc | Dec  deriving (Show)
data Instruction = Instruction ID Modifier Value deriving (Show)

data Predicate = LT | GT | LTE | GTE | EQ | NEQ deriving (Eq, Show)
data Condition = Condition ID Predicate Value deriving (Show)

type Line = (Instruction, Condition)

main :: IO ()
main = do
    filecontent <- readFile "problem8-input.txt" 
    let fileLines = lines filecontent
    let individualWords = map (T.split (== ' ') . T.pack) fileLines
    let registers = createRegisters individualWords
    let instructions = createLines individualWords
    putStrLn $ show $  sortBy compareR $ applyAllInstructions registers instructions


applyInstruction ::  Instruction -> Register -> Register
applyInstruction  (Instruction _ modifier value) (Register id currentValue) = case modifier of
                                                                        Inc -> Register id (currentValue + value)
                                                                        Dec -> Register id (currentValue - value)

applyAllInstructions :: Registers -> [Line] -> Registers
applyAllInstructions regs [] = regs
applyAllInstructions regs ((ins, condition): ls) 
                                    | conditionMatches register condition = applyAllInstructions (map applyInstructionToRegister regs) ls
                                    | otherwise = applyAllInstructions regs ls
                                where (Condition id _ _) = condition
                                      Instruction inid _ _ = ins
                                      register = getRegister id regs
                                      applyInstructionToRegister r = if ((getRegister inid regs)==r) then (applyInstruction ins r) else r

conditionMatches :: Register -> Condition -> Bool
conditionMatches (Register _ currentValue) (Condition _ cond value) = case cond of
                                     Main.LT ->  currentValue < value    
                                     Main.GT ->  currentValue > value    
                                     Main.LTE ->  currentValue <= value    
                                     Main.GTE ->  currentValue >= value    
                                     Main.EQ ->  currentValue == value    
                                     Main.NEQ ->  currentValue /= value    

compareR :: Register -> Register -> Ordering
compareR (Register _ value1 ) (Register _ value2) = compare value2 value1

getRegister :: ID -> Registers -> Register
getRegister id regs = head $ filter (\(Register rid _ ) ->  rid==id) regs
                                     
                                    
-- Parsing stuff
createLines :: [[T.Text]] -> [Line]
createLines ts = map createLine ts

createLine :: [T.Text] -> Line
createLine ts = (Instruction id mod value, Condition condId cond condValue)
                    where ts' = map T.unpack ts
                          id = ts'!!0
                          mod = if ts'!!1 == "inc" then Inc else Dec
                          value = read $ ts'!!2 :: Int
                          condId = ts'!!4
                          cond = parseCond (ts'!!5)
                          condValue = read  $ ts'!!6 :: Int

parseCond :: String -> Predicate
parseCond "<" =  Main.LT
parseCond ">" = Main.GT
parseCond "<=" = Main.LTE
parseCond ">=" = Main.GTE
parseCond "==" = Main.EQ
parseCond "!=" = Main.NEQ 


createRegisters :: [[T.Text]] -> Registers
createRegisters ts = map createEmptyRegister registerNames
                    where registerNames = nub $ foldr (++) (map head ts) []
                          createEmptyRegister x = Register (T.unpack x) 0
