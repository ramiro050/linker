module DB
  ( makeDB
  ) where

import Database.HDBC
import Database.HDBC.Sqlite3

dbpath :: String
dbpath = "../myDB.db"

-- |SQL query to creating a new directory table.
-- Note: This value is only for internal use
newDirTable :: String
newDirTable =
  "CREATE TABLE directory (\
  \file_id INTEGER PRIMARY KEY,\
  \file_path STRING NOT NULL UNIQUE,\
  \last_modified STRING);"

-- |SQL query to creating a new links table.
-- Note: This value is only for internal use
newLinksTable :: String
newLinksTable =
  "CREATE TABLE links (\
  \source_id INTEGER,\
  \dest_id INTEGER);"

-- |Transaction that creates a directory and links table.
makeDB :: IConnection conn => conn -> IO ()
makeDB conn = do
  dirRowMods <- run conn newDirTable []
  putStrLn $ "Rows modified for dir table: " ++ (show dirRowMods)
  linksRowMods <- run conn newLinksTable []
  putStrLn $ "Rows modified for links table: " ++ (show linksRowMods)

main :: IO ()
main = do
  conn <- connectSqlite3 dbpath
  withTransaction conn makeDB
  disconnect conn
