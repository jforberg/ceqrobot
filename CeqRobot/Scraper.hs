{-# LANGUAGE OverloadedStrings #-}

module CeqRobot.Scraper
( scrapeCourses
)
where

import Control.Monad
import qualified Data.ByteString.Lazy as LBS
import Data.Function
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Encoding
import Network.HTTP.Conduit
import Text.HTML.Scalpel
import Text.HTML.TagSoup

import CeqRobot.Model
import CeqRobot.Database

baseUrl = "https://kurser.lth.se/lot/?lasar=17_18&sort1=lp&sort2=slut_lp&sort3=namn&prog=F&forenk=t&val=program&soek=t"

--baseUrl2 = "http://www.ceq.lth.se/rapporter/2016_VT/LP1/FFFF05_2016_VT_LP1_slutrapport.html"
baseUrl2 = "http://www.ceq.lth.se/rapporter/2016_HT/LP2/FAFA05_2016_HT_LP2_slutrapport.html"

test = scrapeCourses baseUrl

{-
test = do
    conn <- getConn
    res <- scrapeCourses baseUrl
    let cs = nubBy ((==) `on` courseCode) . concat . map snd $ res
    forM_ cs $ \c -> do
        insertCourse conn c
        TIO.putStrLn $ courseName c
-}

test2 :: IO (Maybe Ceq)
test2 = do
    rs <- fromJust . ceqRows <$> loadTagsLatin1 baseUrl2
    return . ceqFromMap "asdf12" "2017_HT_LP1" "http://asdfs" . mapFromRows $ rs

scrapeCourses :: Text -> IO [(Text, [Course])]
scrapeCourses url = catMaybes . map scrapeTable . tablesTags <$> loadTagsUtf8 url

loadHttp decoder url = decoder . LBS.toStrict <$> simpleHttp (T.unpack url)

loadTagsUtf8 = (parseTags <$>) . loadHttp decodeUtf8

loadTagsLatin1 = (parseTags <$>) . loadHttp decodeLatin1

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
        Just $ Course { courseCode = readCode code
                      , courseCredits = readCredits credits
                      , courseLevel = readLevel level
                      , courseName = name
                      , courseComment = readComment comment
                      , courseYear = readYear year
                      , coursePeriod = Periodical
                      }
    -- Periodic course, elective
    ("periodiserad", [code, credits, level, _, year, _, _, name, comment, _, _, _]) ->
        Just $ Course { courseCode = readCode code
                      , courseCredits = readCredits credits
                      , courseLevel = readLevel level
                      , courseName = name
                      , courseComment = readComment comment
                      , courseYear = readYear year
                      , coursePeriod = Periodical
                      }
    -- Table headers
    (_, []) ->
        Nothing
    -- Basic courses
    (_, [code, credits, level, _, _, name, comment, _, _, lp1, lp2, lp3, lp4]) ->
        Just $ Course { courseCode = readCode code
                      , courseCredits = readCredits credits
                      , courseLevel = readLevel level
                      , courseName = name
                      , courseComment = readComment comment
                      , courseYear = Nothing
                      , coursePeriod = readPeriod lp1 lp2 lp3 lp4
                      }
    -- Masters courses
    (_, [code, credits, level, _, _, year, _, _, name, comment, _, _, lp1, lp2, lp3, lp4]) ->
        Just $ Course { courseCode = readCode code
                      , courseCredits = readCredits credits
                      , courseLevel = readLevel level
                      , courseName = name
                      , courseComment = readComment comment
                      , courseYear = readYear year
                      , coursePeriod = readPeriod lp1 lp2 lp3 lp4
                      }
    -- Elective courses
    (_, [code, credits, level, _, year, _, _, name, comment, _, _, lp1, lp2, lp3, lp4]) ->
        Just $ Course { courseCode = readCode code
                      , courseCredits = readCredits credits
                      , courseLevel = readLevel level
                      , courseName = name
                      , courseComment = readComment comment
                      , courseYear = readYear year
                      , coursePeriod = readPeriod lp1 lp2 lp3 lp4
                      }
readCode = T.toUpper

readCredits = let f ',' = '.'; f c = c in read . map f . T.unpack

readComment = T.strip . T.drop 1

readYear = Just . toEnum . subtract 1 . read . T.unpack

readPeriod lp1 lp2 lp3 lp4 = Lp (lp1 /= "") (lp2 /= "") (lp3 /= "") (lp4 /= "")

readLevel "G1" = LevelG1
readLevel "G2" = LevelG2
readLevel "A" = LevelA

ceqRows ts = map (map T.strip) <$> scrape rows ts
    where rows = chroots ("tr" @: []) row
          row = texts ("td" @: [])

mapFromRows :: [[Text]] -> Map Text [Text]
mapFromRows = M.fromList . entries
    where entries [] = []
          entries ([]:rs) = entries rs
          entries ((c:cs):rs) = (c, cs) : entries rs

ceqFromMap :: Text -> Text -> Text -> Map Text [Text] -> Maybe Ceq
ceqFromMap code period url m = do
    let tread = read . T.unpack

    -- translate from form <td>123 / 49%</td>
    let readNum :: Text -> Int
        readNum = tread . fst . T.breakOn " / "

    -- translate from form <td>+123</td><td>49</td>
    let readTwo :: [Text] -> Maybe (Int, Int)
        readTwo [a, b] = Just (tread $ T.drop 1 a, tread b)
        readTwo _ = Nothing

    reg_ <-  m M.!? "Antal registrerade på kursen"
    reg <- tread <$> listToMaybe reg_

    pass_ <- m M.!? "Antal godkända/andel av registrerade"
    pass <- readNum <$> listToMaybe pass_

    resp_ <- m M.!? "Antal enkätsvar/svarsfrekvens"
    resp <- readNum <$> listToMaybe resp_

    rel_ <- m M.!? "Kursen känns angelägen för min utbildning"
    (relmean, reldev) <- readTwo rel_

    sat_ <- m M.!? "Överlag är jag nöjd med den här kursen"
    (satmean, satdev) <- readTwo sat_

    return $ Ceq
        { ceqCourseCode = code
        , ceqPeriod = period
        , ceqUrl = url
        , ceqStudentsReg = reg
        , ceqStudentsPass = pass
        , ceqStudentsResp = resp
        , ceqRelevancyMean = relmean
        , ceqRelevancyStddev = reldev
        , ceqSatisfactionMean = satmean
        , ceqSatisfactionStddev = satdev
        }
