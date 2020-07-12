module DB
  ( makeDB
  ) where

import Database.HDBC
import Database.HDBC.Sqlite3

dbpath :: String
dbpath = "../myDB.db"

dbConnection :: IO Connection
dbConnection = connectSqlite3 dbpath

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
makeDBTrans :: IConnection conn => conn -> IO ()
makeDBTrans conn = mapM_ runSimple [newDirTable, newLinksTable]
  where runSimple s = run conn s []

-- |Make directory and links tables in database given by 'dbConnection'.
makeDB :: IO ()
makeDB = do
  conn <- dbConnection
  withTransaction conn makeDBTrans
  disconnect conn

main :: IO ()
main = do
  conn <- dbConnection
  tables <- getTables conn
  putStrLn (show tables)
  disconnect conn
