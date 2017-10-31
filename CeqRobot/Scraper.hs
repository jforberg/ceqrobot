{-# LANGUAGE OverloadedStrings #-}

module CeqRobot.Scraper
( genLotUrl
, genCeqUrl
, scrapeLot
, scrapeCeq
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
import Text.Read
import Text.HTML.Scalpel
import Text.HTML.TagSoup

import CeqRobot.Model
import CeqRobot.Database
import CeqRobot.Util

genLotUrl :: Int -> Text
genLotUrl y =
    "https://kurser.lth.se/lot/?lasar={}_{}&sort1=lp&sort2=slut_lp&sort3=namn&prog=F&forenk=t&val=program&soek=t" `format`
    (y `mod` 100, (y + 1) `mod` 100)

genCeqUrl :: Text -> Period -> Text
genCeqUrl code (Period year sem p) =
    "http://www.ceq.lth.se/rapporter/{}_{}/LP{}/{}_{}_{}_LP{}_slutrapport.html" `format`
    (year, show sem, p, code, year, show sem, p)

loadHttp decoder url = decoder . LBS.toStrict <$> simpleHttp (T.unpack url)

loadTagsUtf8 = (parseTags <$>) . loadHttp decodeUtf8

loadTagsLatin1 = (parseTags <$>) . loadHttp decodeLatin1

--
-- LOT
--

scrapeLot :: Text -> IO ([(Course, CourseRelation)], [Masters])
scrapeLot url = do
    ts <- tablesTags <$> loadTagsUtf8 url

    let cs = concat . catMaybes . map scrapeCourses $ ts
        ms = catMaybes . map scrapeMasters $ ts

    return (cs, ms)

tablesTags ts = partitions (isTagOpenName "h3") ts

scrapeMasters = scrape masters
    where masters = do
              mcode <- readMasters <$> attr "id" ("h3" @: [])
              mname <- (T.drop 3 . snd . T.breakOn " - ") <$> text ("h3" @: [])
              case mcode of
                  "" -> mzero
                  _ -> return $ Masters "F" mcode mname

readMasters tid | T.take 2 tid == "ak"    = ""
                | otherwise               = T.toUpper tid

scrapeCourses = scrape table
    where table = do
              tid <- attr "id" ("h3" @: [])

              case tid of
                  "exjobb" -> mzero
                  _ -> chroots ("tr" @: []) (course tid)

          course tid = do
              cls <- attr "class" ("tr" @: [])
              tds <- texts ("td" @: [])
              case courseFromRow "f" cls tid tds of
                  Just c -> return c
                  Nothing -> mzero

courseFromRow prog_ cls_ tid_ tds_ =
    let cls = T.strip cls_ -- Used to identify periodical courses
        tid = T.strip tid_ -- Used to distinguish master's/basic courses
        programme = T.strip prog_
        tds = map T.strip tds_

    in case (cls, tds) of
    -- Periodic course, masters
    ("periodiserad", [code, credits, level, _, _, year, _, _, name, comment, _, _, _]) ->
        Just $ genCourseAndRelation ( code
                                    , credits
                                    , level
                                    , Just year
                                    , name
                                    , comment
                                    , True
                                    , ""
                                    , ""
                                    , ""
                                    , ""
                                    , programme
                                    , tid
                                    )
    -- Periodic course, elective
    ("periodiserad", [code, credits, level, _, year, _, _, name, comment, _, _, _]) ->
        Just $ genCourseAndRelation ( code
                                    , credits
                                    , level
                                    , Just year
                                    , name
                                    , comment
                                    , True
                                    , ""
                                    , ""
                                    , ""
                                    , ""
                                    , programme
                                    , tid
                                    )
    -- Table headers
    (_, []) ->
        Nothing
    -- Basic courses
    (_, [code, credits, level, _, _, name, comment, _, _, lp1, lp2, lp3, lp4]) ->
        Just $ genCourseAndRelation ( code
                                    , credits
                                    , level
                                    , Nothing
                                    , name
                                    , comment
                                    , False
                                    , lp1
                                    , lp2
                                    , lp3
                                    , lp4
                                    , programme
                                    , tid
                                    )
    -- Masters courses
    (_, [code, credits, level, _, _, year, _, _, name, comment, _, _, lp1, lp2, lp3, lp4]) ->
        Just $ genCourseAndRelation ( code
                                    , credits
                                    , level
                                    , Just year
                                    , name
                                    , comment
                                    , False
                                    , lp1
                                    , lp2
                                    , lp3
                                    , lp4
                                    , programme
                                    , tid
                                    )
    -- Elective courses
    (_, [code, credits, level, _, year, _, _, name, comment, _, _, lp1, lp2, lp3, lp4]) ->
        Just $ genCourseAndRelation ( code
                                    , credits
                                    , level
                                    , Just year
                                    , name
                                    , comment
                                    , False
                                    , lp1
                                    , lp2
                                    , lp3
                                    , lp4
                                    , programme
                                    , tid
                                    )

-- Mega function from hell. Handles all parsing etc. of course metadata
genCourseAndRelation ( code
                     , credits
                     , level
                     , year
                     , name
                     , comment
                     , periodical
                     , lp1
                     , lp2
                     , lp3
                     , lp4
                     , programme
                     , tid
                     ) =
    ( Course { courseCode = rCode code
             , courseCredits = rCredits credits
             , courseLevel = rLevel level
             , courseName = rName name
             }
    , CourseRelation { courseRelCode = rCode code
                     , courseRelProgramme = rProgramme programme
                     , courseRelType = rType tid
                     , courseRelMasters = readMasters tid
                     , courseRelYear = rYear year tid
                     , courseRelComment = rComment comment
                     , courseRelPeriod = rPeriod periodical lp1 lp2 lp3 lp4
                     }
    )
    where rCode = T.toUpper

          rCredits = let f ',' = '.'; f c = c in read . map f . T.unpack

          rLevel "G1" = LevelG1
          rLevel "G2" = LevelG2
          rLevel "A" = LevelA

          -- tid is like "ak1_O"
          rYear Nothing tid = fromIntegral . tread . T.take 1 . T.drop 2 $ tid
          rYear (Just y) _ = fromIntegral . tread $ y

          rName = id

          -- like "X\n       asdf"
          rComment = T.strip . T.drop 1

          rPeriod True _ _ _ _ = Periodical
          rPeriod False lp1 lp2 lp3 lp4 =
              Lp (lp1 /= "") (lp2 /= "") (lp3 /= "") (lp4 /= "")

          rProgramme = T.toUpper

          rType tid | T.take 2 tid == "ak"   = case (T.takeEnd 1 tid) of
                                                   "O" -> Obligatory
                                                   "A" -> AltObligatory
                    | otherwise              = Elective

--
-- CEQ
--

scrapeCeq :: Text -> IO (Maybe Ceq)
scrapeCeq url = ceqFromMap url . mapFromRows . fromJust . ceqRows <$> loadTagsLatin1 url

ceqRows ts = map (map T.strip) <$> scrape rows ts
    where rows = chroots ("tr" @: []) row
          row = texts ("td" @: [])

mapFromRows :: [[Text]] -> Map Text [Text]
mapFromRows = M.fromList . entries
    where entries [] = []
          entries ([]:rs) = entries rs
          entries ((c:cs):rs) = (c, cs) : entries rs

ceqFromMap :: Text -> Map Text [Text] -> Maybe Ceq
ceqFromMap url m = do
    let ltm = listToMaybe

        -- on the form "123 / 49%"
        rSlashed = r . T.strip . fst . T.breakOn "/"
            where r "" = 0
                  r s = tread s

        -- on the form "+123" or "-123" or "0"
        rSigned s | T.take 1 s == "+"    = tread $ T.drop 1 s
                  | otherwise            = tread s

        -- on the form "FFFF05     <crap>"
        rCode :: Text -> Maybe Text
        rCode = ltm . T.words

        f = (m M.!?)

    -- Required info; failed parse => failed scrape
    code <- f "Kurskod" >>= ltm >>= rCode
    year <- f "Läsår" >>= ltm
    lp <- f "Kursen slutade i läsperiod" >>= ltm
    period <- readCeqPeriod year lp

    -- Optional info; failed parse => Nothing field
    let reg = f "Antal registrerade på kursen" >>= fmap tread . ltm
        pass = f "Antal godkända/andel av registrerade" >>= fmap rSlashed . ltm
        resp = f "Antal enkätsvar/svarsfrekvens" >>= fmap rSlashed . ltm
        qual = f "God undervisning" >>= fmap rSigned . ltm
        goal = f "Tydliga mål" >>= fmap rSigned . ltm
        und = f "Förståelseinriktad examination" >>= fmap rSigned . ltm
        work = f "Lämplig arbetsbelastning" >>= fmap rSigned . ltm
        rel = f "Kursen känns angelägen för min utbildning" >>= fmap rSigned. ltm
        sat = f "Överlag är jag nöjd med den här kursen" >>= fmap rSigned . ltm

    Just $ Ceq { ceqCourseCode = code
               , ceqPeriod = period
               , ceqUrl = url
               , ceqRegistered = reg
               , ceqPassed = pass
               , ceqResponded = resp
               , ceqQuality = qual
               , ceqGoals = goal
               , ceqUnderstanding = und
               , ceqWorkload = work
               , ceqRelevance = rel
               , ceqSatisfaction= sat
               }

readCeqPeriod :: Text -> Text -> Maybe Period
readCeqPeriod year period = do
    -- year on the form "201516"
    let y_ = T.take 4 $ year
    y <- treadMaybe y_

    -- period on the form "VT_LP2"
    let s_ = T.take 2 $ period
    s <- case T.toLower s_ of
        "vt" -> Just VT
        "ht" -> Just HT
        _ -> Nothing

    let p_ = T.drop 3 $ period
    p <- case T.toLower p_ of
        "lp1" -> Just 1
        "lp2" -> Just 2
        _ -> Nothing

    return $ case s of
        HT -> Period y s p
        VT -> Period (y + 1) s p

tread = read . T.unpack

treadMaybe = readMaybe . T.unpack

--
-- Master's
--
