import System.IO
import Control.Monad
import System.IO.Error
import Control.Exception
import System.Environment
import Data.List

{-
    5.3

    Write a program that asks names for three files, and then reads 
    the contents of the first and second file, sorts the resulting lines,
    and writes the sorted contents into the third file. Manage IO Exceptions.
-}



main = task `catch` handler  



task :: IO () 
task = do

    -- Say hello and give instructions
    putStrLn $ "Give 3 filenames (in & in) => (out). Make sure input files exists!"
    
    -- Get input
    f1 <- getLine
    f2 <- getLine
    f3 <- getLine
    
    -- Make file handlers for two input files
    hf1 <- openFile f1 ReadMode
    hf2 <- openFile f2 ReadMode  

    -- Get the contents using handlers
    f1cont <- hGetContents hf1  
    f2cont <- hGetContents hf2
    
    -- Get lines as [Lists] and concatenate into a single list 
    let lns = (lines f1cont) ++ (lines f2cont)
    print lns -- print them, why not?
    
    -- Use sor [from Data.List]
    let sortedLines = sort lns
    
    -- Write to f3. Unlines takes a list of strings and makes a single string
    -- ["aaa", "bbb"] -> "aaa\nbbb"
    writeFile f3 $ unlines sortedLines

    
    -- Close files 
    hClose hf1
    hClose hf2
    
    
    
-- You know what's going on here
handler :: IOError -> IO ()
handler e
    | isDoesNotExistError e = putStr $ "The file doesn't exist!"  
    | otherwise = ioError e