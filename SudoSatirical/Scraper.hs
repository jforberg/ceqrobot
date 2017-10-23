{-# LANGUAGE OverloadedStrings #-}

module SudoSatirical.Scraper
( scrapeArticles
)
where

import Data.Maybe
import qualified Data.Text as Text
import Data.Text (Text)
import Data.Time.Clock
import Data.Time.Format
import Text.HTML.Scalpel

import SudoSatirical.Article

baseUrl = "https://www.sudosatirical.com"

scrapeArticles :: IO [Article]
scrapeArticles = fromMaybe [] <$> scrapeURL baseUrl articles
    where articles :: Scraper Text [Article]
          articles = chroots ("article" @: []) article

          article = do
              url <- attr "href" ("a" @: [])
              time <- attr "datetime" ("time" @: [])
              name <- text ("h1" @: [])
              return $ Article url name (parseIso8601 time)

parseIso8601 :: Text -> UTCTime
parseIso8601 ts = parseTimeOrError False defaultTimeLocale f (Text.unpack ts)
    where f = "%Y-%m-%dT%H:%M:%S%z"
