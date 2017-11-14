{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

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

import Control.Applicative
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
import Text.RE.TDFA.Text
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
                  "" -> empty
                  _ -> return $ Masters prog mcode mname

readMasters tid | T.take 2 tid == "ak"    = ""
                | otherwise               = T.toUpper tid

scrapeCourses prog year = scrape table
    where table = do
              tid <- attr "id" ("h3" @: [])
              ths <- map T.strip <$> chroots ("th" @: []) header

              case tid of
                  "exjobb" -> empty
                  _ -> chroots ("tr" @: []) (course tid ths)

          header = strongHeader <|> lpHeader <|> plainHeader

          strongHeader = text ("strong" @: [])

          lpHeader = do
              str <- text anySelector
              case matchedText $ str ?=~ [re|lp[0-9]|] of
                  Nothing -> empty
                  Just t -> return t

          plainHeader = text anySelector

          course tid ths = do
              cls <- T.strip <$> attr "class" ("tr" @: [])
              tds <- map T.strip <$> texts ("td" @: [])

              case courseFromRow prog year cls tid (rowMap ths tds) of
                  Just c -> return c
                  Nothing -> empty

          rowMap ths tds = M.fromList $ zip ths tds

courseFromRow :: Text -> Int32 -> Text -> Text -> Map Text Text -> Maybe (Course, CourseRelation)
courseFromRow prog validYear cls tid m = do
    let find = (m M.!?)

    code    <- find "Kurskod"    >>= rCode
    credits <- find "Poäng"      >>= rCredits
    level   <- find "Nivå"       >>= rLevel
    name    <- find "Kursnamn"   >>= rName
    comment <- find "Fot\xadnot" >>= rComment -- \xad = soft hyphen

    let lp1 = find "lp1"
        lp2 = find "lp2"
        lp3 = find "lp3"
        lp4 = find "lp4"

    period <- rPeriod cls lp1 lp2 lp3 lp4

    let progYear = find "Ingår i åk"
    year <- fromIntegral <$> rYear progYear tid

    let programme = rProgramme prog
        typ = rType tid
        masters = readMasters tid

    return ( Course code credits level name
           , CourseRelation code programme typ masters comment validYear year period
           )

    where rCode = Just . T.toUpper

          rCredits = let f ',' = '.'; f c = c in readMaybe . map f . T.unpack

          rLevel "G1" = Just LevelG1
          rLevel "G2" = Just LevelG2
          rLevel "A"  = Just LevelA
          rLevel _    = Nothing

          -- tid is like "ak1_O"
          rYear Nothing tid = readMaybe . take 1 . drop 2 . T.unpack $ tid
          rYear (Just y) _  = readMaybe . T.unpack $ y

          rName = Just

          -- like "X\n       asdf"
          rComment = Just . T.strip . T.drop 1

          rPeriod "periodiserad" _ _ _ _ = Just Periodical
          rPeriod _ (Just lp1) (Just lp2) (Just lp3) (Just lp4) =
              Just $ Lp (lp1 /= "") (lp2 /= "") (lp3 /= "") (lp4 /= "")
          rPeriod _ _ _ _ _ = Nothing

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
    code   <- f "Kurskod" >>= ltm >>= rCode
    year   <- f "Läsår" >>= ltm
    lp     <- f "Kursen slutade i läsperiod" >>= ltm
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

