module Lib
    ( someFunc
    ) where

import System.IO

someFunc :: IO ()
someFunc = putStrLn "someFunc"

section :: String
section = "* Backlinks"

printFile :: Handle -> IO ()
printFile hFile = do
  line <- hGetLine hFile
  print line

run :: IO ()
run = withFile "/Users/ramiroleal/haskell/test.org" ReadMode printFile
