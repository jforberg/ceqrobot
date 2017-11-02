{-# LANGUAGE OverloadedStrings #-}

module CeqRobot.Scraper
( genRootUrl
, genLotUrl
, genCeqUrl
, guessCeqPeriod
, scrapeRoot
, scrapeLot
, scrapeCeq
)
where

import Control.Monad
import qualified Data.ByteString.Lazy as LBS
import Data.Function
import Data.Int
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

genLotUrl :: Text -> Int32 -> Text
genLotUrl p y =
    "https://kurser.lth.se/lot/?lasar={}_{}&sort1=lp&sort2=slut_lp&sort3=namn&prog={}&forenk=t&val=program&soek=t" `format`
    (y `mod` 100, (y + 1) `mod` 100, p)

genCeqUrl :: Text -> Period -> Text
genCeqUrl code (Period year sem p) =
    "http://www.ceq.lth.se/rapporter/{}_{}/LP{}/{}_{}_{}_LP{}_slutrapport.html" `format`
    (year, show sem, p, code, year, show sem, p)

genRootUrl :: Text
genRootUrl = "https://kurser.lth.se/lot/?val=program"

guessCeqPeriod :: Int32 -> CoursePeriod -> Maybe Period
guessCeqPeriod year (Lp _ _ _ True) = Just $ Period (year + 1) VT 2
guessCeqPeriod year (Lp _ _ True _) = Just $ Period (year + 1) VT 1
guessCeqPeriod year (Lp _ True _ _) = Just $ Period year HT 2
guessCeqPeriod year (Lp True _ _ _) = Just $ Period year HT 1
guessCeqPeriod _ _ = Nothing

loadHttp decoder url = decoder . LBS.toStrict <$> simpleHttp (T.unpack url)

loadTagsUtf8 = (parseTags <$>) . loadHttp decodeUtf8

loadTagsLatin1 = (parseTags <$>) . loadHttp decodeLatin1

--
-- ROOT: PROGRAMMES, YEARS
--

scrapeRoot :: Text -> IO ([Int32], [Programme])
scrapeRoot url = do
    ts <- loadTagsUtf8 url

    let ys = fromJust $ scrapeYears ts
        ps = fromJust $ scrapeProgrammes ts

    return (ys, ps)

scrapeYears = scrape years
    where years = chroot ("select" @: ["name" @= "lasar"]) years'

          years' = catMaybes . map rYear <$> texts ("option" @: [])

          rYear = readMaybe . T.unpack . fst . T.breakOn "/"

scrapeProgrammes = scrape programmes
    where programmes = chroots ("label" @: []) programme

          programme = do
              code <- attr "value" ("input" @: ["name" @= "prog"])
              name <- rName <$> text (AnyTag @: [])
              return $ Programme (T.toUpper code) name

          rName = T.strip . snd . T.breakOnEnd ":"

--
-- LOT
--

scrapeLot :: Text -> Int32 -> Text -> IO ([(Course, CourseRelation)], [Masters])
scrapeLot prog year url = do
    ts <- tablesTags <$> loadTagsUtf8 url

    let cs = concat . catMaybes . map (scrapeCourses prog year) $ ts
        ms = catMaybes . map (scrapeMasters prog) $ ts

    return (cs, ms)

tablesTags ts = partitions (isTagOpenName "h3") ts

scrapeMasters prog = scrape masters
    where masters = do
              mcode <- readMasters <$> attr "id" ("h3" @: [])
              mname <- (T.drop 3 . snd . T.breakOn " - ") <$> text ("h3" @: [])
              case mcode of
                  "" -> mzero
                  _ -> return $ Masters prog mcode mname

readMasters tid | T.take 2 tid == "ak"    = ""
                | otherwise               = T.toUpper tid

scrapeCourses prog year = scrape table
    where table = do
              tid <- attr "id" ("h3" @: [])

              case tid of
                  "exjobb" -> mzero
                  _ -> chroots ("tr" @: []) (course tid)

          course tid = do
              cls <- attr "class" ("tr" @: [])
              tds <- texts ("td" @: [])
              case courseFromRow prog year cls tid tds of
                  Just c -> return c
                  Nothing -> mzero

-- REFACTOR ME
courseFromRow prog validYear cls_ tid_ tds_ =
    let cls = T.strip cls_ -- Used to identify periodical courses
        tid = T.strip tid_ -- Used to distinguish master's/basic courses
        tds = map T.strip tds_

    in case (cls, tds) of
    -- Periodic course, basic
    ("periodiserad", [code, credits, level, _, _, name, comment, _, _, _]) ->
        Just $ genCourseAndRelation ( code
                                    , credits
                                    , level
                                    , Nothing
                                    , validYear
                                    , name
                                    , comment
                                    , True
                                    , ""
                                    , ""
                                    , ""
                                    , ""
                                    , prog
                                    , tid
                                    )
    -- Periodic course, masters
    ("periodiserad", [code, credits, level, _, _, progYear, _, _, name, comment, _, _, _]) ->
        Just $ genCourseAndRelation ( code
                                    , credits
                                    , level
                                    , Just progYear
                                    , validYear
                                    , name
                                    , comment
                                    , True
                                    , ""
                                    , ""
                                    , ""
                                    , ""
                                    , prog
                                    , tid
                                    )
    -- Periodic course, elective
    ("periodiserad", [code, credits, level, _, progYear, _, _, name, comment, _, _, _]) ->
        Just $ genCourseAndRelation ( code
                                    , credits
                                    , level
                                    , Just progYear
                                    , validYear
                                    , name
                                    , comment
                                    , True
                                    , ""
                                    , ""
                                    , ""
                                    , ""
                                    , prog
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
                                    , validYear
                                    , name
                                    , comment
                                    , False
                                    , lp1
                                    , lp2
                                    , lp3
                                    , lp4
                                    , prog
                                    , tid
                                    )
    -- Masters courses
    (_, [code, credits, level, _, _, progYear, _, _, name, comment, _, _, lp1, lp2, lp3, lp4]) ->
        Just $ genCourseAndRelation ( code
                                    , credits
                                    , level
                                    , Just progYear
                                    , validYear
                                    , name
                                    , comment
                                    , False
                                    , lp1
                                    , lp2
                                    , lp3
                                    , lp4
                                    , prog
                                    , tid
                                    )
    -- Elective courses
    (_, [code, credits, level, _, progYear, _, _, name, comment, _, _, lp1, lp2, lp3, lp4]) ->
        Just $ genCourseAndRelation ( code
                                    , credits
                                    , level
                                    , Just progYear
                                    , validYear
                                    , name
                                    , comment
                                    , False
                                    , lp1
                                    , lp2
                                    , lp3
                                    , lp4
                                    , prog
                                    , tid
                                    )

-- Mega function from hell. Handles all parsing etc. of course metadata
genCourseAndRelation ( code
                     , credits
                     , level
                     , progYear
                     , validYear
                     , name
                     , comment
                     , periodical
                     , lp1
                     , lp2
                     , lp3
                     , lp4
                     , prog
                     , tid
                     ) =
    ( Course { courseCode = rCode code
             , courseCredits = rCredits credits
             , courseLevel = rLevel level
             , courseName = rName name
             }
    , CourseRelation { courseRelCode = rCode code
                     , courseRelProgramme = rProgramme prog
                     , courseRelType = rType tid
                     , courseRelMasters = readMasters tid
                     , courseRelProgYear = rYear progYear tid
                     , courseRelValidYear = validYear
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

--
-- Master's
--
