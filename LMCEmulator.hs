module LMCEmulator (
    Enviroment(..),
    Line(..),
    Label(..),
    runRam,
    readLine,
    readLines,
    runLine,
    getLinesFromFile,
    getLabelsFromFile,
    machineCodeToLine,
    lineToMachineCode,
    assembleLines
) where 

import qualified Data.Map as Map
import Data.Char
import Data.Maybe
import System.IO.Unsafe

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

data Enviroment = Enviroment {
    acc :: Int,
    pc :: Int,
    ram :: Map.Map Int Int  
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
    | m == DAT = 9
    | otherwise = -1

lineToMachineCode :: Line -> Int
lineToMachineCode Line{ label = _, mnemonic = m, address = a}
    | m == INP = 901
    | m == OUT = 902
    | m == HLT = 0
    | otherwise = read $ show(mnemonicToInt m) ++ show(fromJust(a))

readLinesProg :: [String] -> Int -> [Maybe Label] -> [Line]
readLinesProg [] n labels =  []
readLinesProg (x:xs) n labels = 
    [readLine x n labels] ++ readLinesProg xs (n+1) labels

readLines :: [String] -> [Maybe Label] -> [Line]
readLines xs = readLinesProg xs 0 

getLabelsFromFile :: String -> IO [Maybe Label]
getLabelsFromFile filename = do
    contents <- readFile filename
    let ls = getLabels $ lines contents
    return ls

getLinesFromFile :: String -> IO [Line]
getLinesFromFile filename = do
    labels <- getLabelsFromFile filename 
    contents <- readFile filename
    let ls = readLines (lines contents) labels
    return ls

assembleLinesProg :: [Line] -> Int -> [(Int, Int)]
assembleLinesProg ls n
    | length ls > n = [(n, lineToMachineCode (ls!!n))] ++ assembleLinesProg ls (n+1)
    | otherwise = []  

assembleLines :: [Line] -> Enviroment
assembleLines ls = Enviroment{ acc = 0, pc = 0, ram = Map.fromList $ assembleLinesProg ls 0}

accHelper :: Enviroment -> Int -> (Enviroment -> Int -> Int) -> Enviroment
accHelper env adr f = Enviroment { acc = (f env adr), pc = pc env + 1, ram = ram env}

add :: Enviroment -> Int -> Enviroment
add env adr = accHelper env adr (\env adr -> (acc env) + (fromJust $ Map.lookup adr $ ram env))

sub :: Enviroment -> Int -> Enviroment
sub env adr = accHelper env adr (\env adr -> (acc env) - (fromJust $ Map.lookup adr $ ram env))

lda :: Enviroment -> Int -> Enviroment
lda env adr = accHelper env adr (\env adr -> fromJust $ Map.lookup adr $ ram env)

sta :: Enviroment -> Int -> Enviroment
sta env adr = Enviroment { 
    acc = acc env, 
    pc = pc env + 1, 
    ram = Map.insert adr (acc env) $ ram env
}

pcHelper :: Enviroment -> Int -> (Enviroment -> Int -> Int) -> Enviroment
pcHelper env adr f = Enviroment { acc = acc env, pc = f env adr, ram = ram env}

brp :: Enviroment -> Int -> Enviroment
brp env adr = pcHelper env adr (\env adr -> if (acc env) >= 0 then 
                                              fromJust $ Map.lookup adr $ ram env 
                                            else pc env
                               ) 

brz :: Enviroment -> Int -> Enviroment
brz env adr = pcHelper env adr (\env adr -> if (acc env) == 0 then 
                                              fromJust $ Map.lookup adr $ ram env 
                                            else pc env
                               ) 

bra :: Enviroment -> Int -> Enviroment
bra env adr = pcHelper env adr (\env adr -> fromJust $ Map.lookup adr $ ram env)

hlt :: Enviroment -> Int -> Enviroment
hlt env adr = pcHelper env adr (\env adr -> -1)

inp :: Enviroment -> IO Enviroment 
inp env = do
    val <- readLn :: IO Int
    return Enviroment{acc = val, pc = pc env + 1, ram = ram env}

out :: Enviroment -> IO Enviroment 
out env = do
    putStrLn $ show (acc env)
    return Enviroment{acc = acc env, pc = pc env + 1, ram = ram env} 

runLine :: Line -> Enviroment -> Enviroment 
runLine Line{label = _, mnemonic = m, address = a} env 
    | m == HLT = hlt env adr 
    | m == ADD = add env adr
    | m == SUB = sub env adr
    | m == STA = sta env adr
    | m == LDA = lda env adr
    | m == BRP = brp env adr
    | m == BRA = bra env adr
    | m == BRZ = brz env adr
    | m == INP = unsafePerformIO $ inp env
    | m == OUT = unsafePerformIO $ out env
    | otherwise = env
    where adr = fromJust a

runRam :: Enviroment -> Enviroment
runRam env 
    | val == Nothing = runLine Line{label = Nothing, mnemonic = HLT, address= Nothing} env
    | pc env /= -1 = runLine (machineCodeToLine(fromJust(val))) $ runRam env
    | otherwise = env
    where val = Map.lookup (pc env) (ram env)
