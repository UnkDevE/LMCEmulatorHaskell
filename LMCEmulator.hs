module LMCEmulator (
) where 

import qualified Data.Map as Map
import Data.Char
import Data.Maybe

data Mnemonic = HLT | ADD | SUB | STA | LDA | BRA | BRZ | BRP | INP | OUT | DAT
    deriving ( Eq, Ord, Show, Read, Bounded, Enum)

data Label = Label{
    name :: String,
    adr :: Int   
} deriving (Show, Read, Eq)

data Line = Line {
    label :: Maybe Label,
    mnemonic :: Mnemonic,
    address :: Maybe Int
} deriving (Show, Read, Eq)

first :: [a] -> a
first (x:_) = x

second :: [a] -> a
second (_:x:_) = x

third :: [a] -> a
third (_:_:x:_) = x

isDigitStr :: String -> Bool
isDigitStr [x] = isDigit(x)
isDigitStr (x:xs) = isDigit(x) && isDigitStr(xs)

toUpperStr :: String -> String
toUpperStr [x] = [toUpper(x)]
toUpperStr (x:xs) = toUpper(x):toUpperStr(xs)

intToMnemonic :: Int -> Mnemonic 
intToMnemonic i  
    | i == 0 = HLT
    | i == 1 = ADD
    | i == 2 = SUB
    | i == 3 = STA
    | i == 5 = LDA
    | i == 6 = BRA
    | i == 7 = BRZ
    | i == 8 = BRP

mnemonics = ["HLT", "ADD", "SUB", "STA", "LDA", "BRA", "BRZ", "BRP", "INP", "OUT", "DAT"]

isMnemonic m = elem m mnemonics

labelHelper :: [String] -> Int -> Label
labelHelper strs lineno
    | (toUpperStr $ second strs) == "DAT" = Label {
        name = first strs, 
        adr = read $ third strs :: Int
    }
    | otherwise = Label {
        name = first strs,
        adr = lineno
    }

getLabel :: String -> Int -> Maybe Label
getLabel str lineno
    | isMnemonic $ head strs = Nothing
    | otherwise = Just(labelHelper strs lineno)
    where strs = words str

getLabelsProg :: [String] -> Int -> [Maybe Label]
getLabelsProg [] n = []
getLabelsProg (str:strs) n =
    [getLabel str n ] ++ getLabelsProg strs (n+1)

getLabels :: [String] -> [Maybe Label]
getLabels strs = getLabelsProg strs 0


maybeName :: Maybe Label -> Maybe String
maybeName label 
    | label == Nothing = Nothing
    | otherwise = Just(name $ fromJust label)

lookupAdr :: [Maybe Label] -> String -> Int
lookupAdr labels name = adr $ fromJust $ head [ x | x <- labels, maybeName x == Just name]

readAddress :: String -> [Maybe Label] -> Int
readAddress str labels
    | isDigitStr str = read str :: Int
    | otherwise = lookupAdr labels str

readThreeWords :: [String] -> Int -> [Maybe Label] -> Line
readThreeWords strs lineNo labels
    | mnemonic == DAT = Line { 
                label = Just Label{name = first strs, adr = address}, 
                mnemonic = DAT, 
                address = Just address 
            }
    | otherwise = Line {
            label = Just Label{name = first strs, adr = lineNo},
            mnemonic = mnemonic,
            address = Just address
        }
    where mnemonic = read $ toUpperStr $ second strs :: Mnemonic
          address = readAddress (third strs) labels


readTwoWords :: [String] -> Int -> [Maybe Label] -> Line
readTwoWords strs lineNo labels
    | isMnemonic $ toUpperStr $ first strs = Line{
            label = Nothing,
            mnemonic = read $ toUpperStr $ first strs :: Mnemonic,
            address = Just (readAddress (second strs) labels)
        }
    | otherwise = Line {
            label = Just Label{name = first strs, adr = lineNo},
            mnemonic = read $ toUpperStr str2 :: Mnemonic,
            address = Nothing
        }
    where str2 = second strs

readLine :: String -> Int -> [Maybe Label] -> Line
readLine str lineNo labels
    | length strs < 2 = Line{
            label = Nothing,
            mnemonic = read $ toUpperStr $ first strs :: Mnemonic,
            address = Nothing
        }
    | length strs < 3 = readTwoWords strs lineNo labels
    | otherwise = readThreeWords strs lineNo labels
    where strs = words str

machineCodeToLine :: Int -> Line
machineCodeToLine machineCode
    | machineCode == 901 = Line { label = Nothing, mnemonic = INP, address = Nothing}
    | machineCode == 902 = Line { label = Nothing, mnemonic = OUT, address = Nothing}
    | otherwise =  Line { 
            label = Nothing, 
            mnemonic = intToMnemonic (digitToInt $ head $ show machineCode :: Int), 
            address = Just (read $ tail $ show machineCode :: Int)
        }

mnemonicToInt :: Mnemonic -> Int
mnemonicToInt m 
    | m == HLT = 0
    | m == ADD = 1
    | m == SUB = 2
    | m == STA = 3
    | m == LDA = 5
    | m == BRA = 6
    | m == BRZ = 7
    | m == BRP = 8
    | otherwise = -1

lineToMachineCode :: Line -> Int
lineToMachineCode Line{ label = _, mnemonic = m, address = a}
    | m == INP = 901
    | m == OUT = 902
    | otherwise = read $ show(mnemonicToInt m) ++ show(fromJust(a))

readLinesProg :: [String] -> Int -> [Maybe Label] -> [Line]
readLinesProg [] n labels =  []
readLinesProg (x:xs) n labels = 
    [readLine x n labels] ++ readLinesProg xs (n+1) labels

readLines :: [String] -> [Maybe Label] -> [Line]
readLines xs = readLinesProg xs 0 

getLabelsFromFile filename = do
    contents <- readFile filename
    let ls = getLabels $ lines contents
    return ls

getLinesFromFile filename = do
    labels <- getLabelsFromFile filename 
    contents <- readFile filename
    let ls = readLines (lines contents) labels
    return ls

