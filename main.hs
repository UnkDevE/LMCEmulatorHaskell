import LMCEmulator

main :: IO()
main = do
    putStrLn "LMC file to run:"
    filename <- getLine
    lines <- getLinesFromFile filename
    let env = assembleLines lines
    runRam env
    putStrLn "program terminated"