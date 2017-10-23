{-# LANGUAGE OverloadedStrings #-}

module SudoSatirical.Control
( run
)
where

import Prelude hiding (log)

import Control.Concurrent
import Control.Monad
import Data.List
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Data.Time.Format
import Data.Time.LocalTime
import System.IO
import Text.Printf

import SudoSatirical.Article
import SudoSatirical.Database
import SudoSatirical.Reddit
import SudoSatirical.Scraper

data State = State
    { stDatabase :: Database
    }
    deriving Show

sleepUs = 10 * 60 * 1000 * 1000

run dbFilename = do
    db <- openDatabase dbFilename
    log "OK, running..."
    mainLoop $ State db

mainLoop st = do
    let db = stDatabase st

    as <- reverse <$> scrapeArticles

    when (as == []) $ log "Warning: got empty scrape result."

    newas <- filterM (fmap not . checkSeenArticle db) as

    case newas of
        [] -> return ()
        (newa:_) -> handleNewArticle st newa

    threadDelay sleepUs
    mainLoop st

handleNewArticle st a = do
    let db = stDatabase st
        req = SubmitRequest
                  { srKind = "link"
                  , srSubreddit = "sudosatirical"
                  , srTitle = articleName a
                  , srUrl = articleUrl a
                  }

    Right redditToken <- getToken

    res <- submitLink redditToken req

    log $ showArticle a

    case res of
        Left re -> log $ showRedditError re
        Right _ -> recordSeenArticle db a

showArticle a = Text.concat [ articleName a, " (", articleUrl a, ")" ]

showRedditError (RedditError t msg) = Text.concat [ t, ": ", msg ]

log :: Text -> IO ()
log s = do
    TIO.putStrLn s
    hFlush stdout
