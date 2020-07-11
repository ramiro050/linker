module Lib
  ( someFunc
  ) where

import OrgParser
import System.IO
import Text.Parsec

someFunc :: IO ()
someFunc = putStrLn "someFunc"

--findSectionIn :: String -> String -> String
--findSectionIn s text = 

--findSection :: String -> Handle -> IO ()
--findSection s hFile = do
--  line <- hGetLine hFile
--  case parse (sectionParser s) "" line of
--    Left _ -> findSection s hFile
--    Right _ -> putStr ("Section " ++ s ++ " found!\n")

{-
getLinks :: Handle -> IO [String]
getLinks hFile = do
  eof <- hIsEOF hFile
  if eof then return [] else
    do
      line <- hGetLine hFile
      case parse linkParser "" line of
        Left _ -> getLinks hFile
        Right (_, l) -> (l:) <$> getLinks hFile

run :: IO ()
run = withFile "/Users/ramiroleal/haskell/test.org" ReadMode hComp
  where
    hComp h = do
      findSection "Backlinks" h
      ls <- getLinks h
      print ls
-}
