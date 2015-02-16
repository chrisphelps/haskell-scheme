module Main where
import System.Environment
 
main :: IO ()
main = do
    (command:args) <- getArgs
    dispatch command args

dispatch "prompt" args = promptForName args
dispatch "total" args = total args
dispatch command args = unknown command args
    
total :: [String] -> IO ()
total args = do
    -- args <- cmdline
    let nums = fmap read args :: [Int] -- hint that we want [Int]
        total = foldl (+) 0 nums
    putStrLn ("Total = " ++ show total)

promptForName :: [String] -> IO ()
promptForName cmdline = do
    putStrLn "Enter a name"
    name <- getLine
    putStrLn ("Hello, " ++ name)

unknown :: String -> [String] -> IO ()
unknown command args = do
    putStrLn ("Unknown command " ++ command) 
