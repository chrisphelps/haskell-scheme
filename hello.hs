module Main where
import System.Environment
 
main :: IO ()
main = do
    args <- getArgs
    let nums = fmap read args :: [Int] -- hint that we want [Int]
    	total = foldl (+) 0 nums
    putStrLn ("Total = " ++ show total)

    
