{-# LANGUAGE OverloadedStrings #-}

module CeqRobot.ScrapeCourse
( scrapeCourses
)
where

import Control.Monad
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding
import Network.HTTP.Conduit
import Text.HTML.Scalpel
import Text.HTML.TagSoup

import CeqRobot.Course

baseUrl = "https://kurser.lth.se/lot/?lasar=17_18&sort1=lp&sort2=slut_lp&sort3=namn&prog=F&forenk=t&val=program&soek=t"

test = scrapeCourses baseUrl

scrapeCourses :: Text -> IO [(Text, [Course])]
scrapeCourses url = catMaybes . map scrapeTable . tablesTags <$> loadTags url

loadHttp url = decodeUtf8 . LBS.toStrict <$> simpleHttp (T.unpack url)

loadTags = (parseTags <$>) . loadHttp

tablesTags ts = partitions (isTagOpenName "h3") ts

scrapeTable ts = scrape table ts
    where table = do
              cat <- attr "id" ("h3" @: [])

              case cat of
                  "exjobb" -> mzero
                  _ -> do
                      courses <- chroots ("tr" @: []) course
                      return (cat, courses)

          course = do
              cls <- attr "class" ("tr" @: [])
              as <- texts ("td" @: [])
              case courseFromRow cls as of
                  Just c -> return c
                  Nothing -> mzero

courseFromRow cls as = let as_ = map T.strip as; cls_ = T.strip cls in
    case (cls_, as_) of
    -- Periodic course, masters
    ("periodiserad", [code, credits, level, _, _, year, _, _, name, comment, _, _, _]) ->
        Just $ Course { courseCode = code
                      , courseCredits = readCredits credits
                      , courseLevel = readLevel level
                      , courseName = name
                      , courseComment = readComment comment
                      , courseYear = readYear year
                      }
    -- Periodic course, elective
    ("periodiserad", [code, credits, level, _, year, _, _, name, comment, _, _, _]) ->
        Just $ Course { courseCode = code
                      , courseCredits = readCredits credits
                      , courseLevel = readLevel level
                      , courseName = name
                      , courseComment = readComment comment
                      , courseYear = readYear year
                      }
    -- Table headers
    (_, []) ->
        Nothing
    -- Basic courses
    (_, [code, credits, level, _, _, name, comment, _, _, lp1, lp2, lp3, lp4]) ->
        Just $ Course { courseCode = code
                      , courseCredits = readCredits credits
                      , courseLevel = readLevel level
                      , courseName = name
                      , courseComment = readComment comment
                      , courseYear = Nothing
                      }
    -- Masters courses
    (_, [code, credits, level, _, _, year, _, _, name, comment, _, _, lp1, lp2, lp3, lp4]) ->
        Just $ Course { courseCode = code
                      , courseCredits = readCredits credits
                      , courseLevel = readLevel level
                      , courseName = name
                      , courseComment = readComment comment
                      , courseYear = readYear year
                      }
    -- Elective courses
    (_, [code, credits, level, _, year, _, _, name, comment, _, _, lp1, lp2, lp3, lp4]) ->
        Just $ Course { courseCode = code
                      , courseCredits = readCredits credits
                      , courseLevel = readLevel level
                      , courseName = name
                      , courseComment = readComment comment
                      , courseYear = readYear year
                      }

readCredits = let f ',' = '.'; f c = c in read . map f . T.unpack

readComment = T.strip . T.drop 1

readYear = Just . toEnum . subtract 1 . read . T.unpack

readLevel "G1" = LevelG1
readLevel "G2" = LevelG2
readLevel "A" = LevelA

