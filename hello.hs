module Main where
import System.Environment
 
main :: IO ()
--main = total getArgs
main = promptForName getArgs
    
total :: IO [String] -> IO ()
total cmdline = do
    args <- cmdline
    let nums = fmap read args :: [Int] -- hint that we want [Int]
        total = foldl (+) 0 nums
    putStrLn ("Total = " ++ show total)

promptForName :: IO [String] -> IO ()
promptForName cmdline = do
    putStrLn "Enter a name"
    name <- getLine
    putStrLn ("Hello, " ++ name)
