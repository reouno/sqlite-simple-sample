{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( testCreate
    , testSQLite
    , testSQLite2
    ) where

import Control.Applicative
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.String (fromString)
import Data.Time.Clock
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow

data TestField = TestField Int String deriving (Show)

instance FromRow TestField where
    fromRow = TestField <$> field <*> field

testSQLite :: IO ()
testSQLite = do
    -- connect to database
    conn <- open "test.db"

    -- create and execute query
    execute conn "INSERT INTO test (str) VALUES (?)"
        (Only ("test string2" :: String))
    r <- query_ conn "SELECT * from test" :: IO [TestField]
    mapM_ print r
    close conn

testSQLite2 :: IO ()
testSQLite2 = do
    let table = "test"
    conn <- open "test.db"

    putStrLn "delete all rows"
    deleteAll conn table
    r1 <- selectAll conn table
    mapM_ print r1

    --let newStr = "inserted new str" :: Text
    putStrLn "insert row"
    insert conn table "inserted new str"
    r2 <- selectAll conn table
    mapM_ print r2

    let id' = 1
    updateStr <- getCurrentTime
    putStrLn "update row"
    update conn table id' $ show updateStr
    r3 <- selectAll conn table
    mapM_ print r3
    close conn

testCreate :: IO ()
testCreate = do
    conn <- open "test.db"
    putStrLn "create table \"sample\""
    execute_ conn "CREATE TABLE IF NOT EXISTS sample (id INTEGER PRIMARY KEY AUTOINCREMENT, str TEXT)"
    close conn

insert :: Connection -> String -> String -> IO ()
insert conn table str =
    let
        qstr = fromString ("INSERT INTO "
                    ++ table
                    ++ " (str) "
                    ++ "VALUES (?)") :: Query
    in
        execute conn qstr (Only (str :: String))

selectAll :: Connection -> String -> IO [TestField]
selectAll conn table =
    query_ conn "SELECT * from test" :: IO [TestField]

update :: Connection -> String -> Int -> String -> IO ()
update conn table id' str =
    execute conn (fromString ("UPDATE "
              ++ table
              ++ " SET str = (?) WHERE id = (?)"))
        (str :: String, id' :: Int)

delete :: Connection -> String -> Int -> IO ()
delete conn table id' =
    execute conn (fromString ("DELETE from "
               ++ table
               ++ " WHERE id = (?)"))
        (Only (id' :: Int))

deleteAll :: Connection -> String -> IO ()
deleteAll conn table =
    execute_ conn $ fromString ("DELETE from " ++ table)
