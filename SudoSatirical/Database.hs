{-# LANGUAGE OverloadedStrings #-}

module SudoSatirical.Database
( Database(..)
, openDatabase
, recordSeenArticle
, checkSeenArticle
)
where

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock
import Data.Time.Format
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow

import SudoSatirical.Article

newtype Database = Database Connection

instance Show Database where
    show = const "[sqlite connection]"

instance ToRow Article where
    toRow (Article url name t) = toRow (url, name, formatTime defaultTimeLocale timeFormat t)

instance FromRow Int where
    fromRow = field

timeFormat = "%Y-%m-%dT%H:%M:%SZ"

openDatabase :: String -> IO Database
openDatabase location = do
    conn <- open location
    execute_ conn
        "create table if not exists articles(url text primary key, name text not null, timestamp text not null)"
    return $ Database conn

recordSeenArticle :: Database -> Article -> IO ()
recordSeenArticle (Database conn) a = do
    t <- getCurrentTime
    execute conn
        "insert into articles(url, name, timestamp) values (?, ?, ?)" a

checkSeenArticle :: Database -> Article -> IO Bool
checkSeenArticle (Database conn) a = do
    let url = articleUrl a
        q = "select 1 as t from articles where url = ? limit 1"
        params = Only url

    res <- query conn q params :: IO [Int]
    return . not . null $ res

