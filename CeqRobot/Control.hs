{-# LANGUAGE OverloadedStrings #-}

module CeqRobot.Control
( InitQueue(..)
, run
)
where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Network.HTTP.Client
import Network.HTTP.Conduit
import System.IO

import CeqRobot.Database
import CeqRobot.Model
import CeqRobot.Scraper
import CeqRobot.Util

data InitQueue = YesInitQueue | NoInitQueue
    deriving (Show, Read, Eq)

lotPrio = 1.0
ceqPrio = 2.0

scrapeDelay = 500 * 1000

run :: InitQueue -> IO ()
run iq = do
    TIO.putStrLn "Open database connection.."
    conn <- getConn

    when (iq == YesInitQueue) $ do
        TIO.putStrLn "Scraping root page..."
        void $ performScrape conn $ ScrapeJob "root" genRootUrl Nothing

    TIO.putStrLn "Beginning main scrape pass..."
    runIteration conn

runIteration conn = do
   msj <- peekQueueScrape conn

   case msj of
       Nothing -> return ()
       Just sj@(ScrapeJob t ref meta) -> do
           performScrape conn sj
           dequeueScrape conn sj

           hFlush stdout

           threadDelay scrapeDelay
           runIteration conn

performScrape conn sj@(ScrapeJob t ref _) = do
    logScrape t ref "begin"

    ret <- try $ case t of
        "root" -> performScrapeRoot conn sj
        "lot" -> performScrapeLot conn sj
        "ceq" -> performScrapeCeq conn sj

    case ret of
        Left (HttpExceptionRequest _ _) -> do
            logScrape t ref "fail/http"
            return False
        Right False -> do
            logScrape t ref "fail/parse"
            return False
        Right True -> do
            logScrape t ref "ok"
            return True

performScrapeRoot conn (ScrapeJob _ url _) = do
    (years, programmes) <- scrapeRoot url

    forM_ programmes $ \p -> do
        print p
        forM_ years $ \y -> do
            let url = genLotUrl (programmeCode p) y
                meta = tshow (programmeCode p, y)

            insertProgramme conn p
            queueScrape conn lotPrio $ ScrapeJob "lot" url (Just meta)
            print (p, y)

    return $ not (null years) && not (null programmes)

performScrapeLot conn (ScrapeJob _ url meta) = do
    let (prog, year) = tread . fromJust $ meta

    (coursesRelations, masters) <- scrapeLot prog year url

    forM_ masters $ \m -> do
        insertMasters conn m
        print m

    forM_ coursesRelations $ \(c, r) -> do
        insertCourse conn c
        print c

        insertCourseRelation conn r
        print r

        let mperiod = guessCeqPeriod year (courseRelPeriod r)

        case mperiod of
            Nothing -> return ()
            Just period -> do
                let url = genCeqUrl (courseCode c) period
                queueScrape conn ceqPrio $ ScrapeJob "ceq" url Nothing

    return $ not (null masters) && not (null coursesRelations)

performScrapeCeq conn (ScrapeJob _ url _) = do
    mq <- scrapeCeq url

    case mq of
        Just q -> insertCeq conn q >> print q >> return True
        Nothing -> return False

logScrape :: Text -> Text -> Text -> IO ()
logScrape t url stat = TIO.putStrLn $ "[{}] {} {}" `format` (t, T.toUpper stat, url)
