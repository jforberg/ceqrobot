{-# LANGUAGE OverloadedStrings #-}

module CeqRobot.Control
( run
)
where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Network.HTTP.Client

import CeqRobot.Database
import CeqRobot.Model
import CeqRobot.Scraper
import CeqRobot.Util

fromYear = 2010
toYear = 2017

run :: IO ()
run = do
    conn <- getConn
    initQueueIfNeeded conn
    runIteration conn

runIteration conn = do
   m <- peekQueueScrape conn
   case m of
       Nothing -> return ()
       Just (t, ref) -> do
           res <- performScrapeCeq conn ref
           dequeueScrape conn t ref
           threadDelay 2000000
           runIteration conn

initQueueIfNeeded conn = do
    js <- peekQueueScrape conn

    when (js == Nothing) $ do
        forM_ [fromYear..toYear] $ \y -> do
            when (y /= toYear) $ do
                deleteCourseRelations conn

            let url = genLotUrl y
            (cs, ms) <- scrapeLot url
            logScrape "course" url "ok"

            forM_ ms $ \m -> do
                insertMasters conn m
                print m

            forM_ cs $ \(c, r) -> do
                insertCourse conn c
                insertCourseRelation conn r
                queueScrape conn "ceq" (courseCode c)
                print (c, r)

performScrapeCeq conn code = do
    res <- forM periodPerms $ \p -> do
        let url = genCeqUrl code p
        ret <- tryPerformScrapeCeq url

        case ret of
            Left _ -> do
                logScrape "ceq" url "fail/http"
                return False
            Right Nothing -> do
                logScrape "ceq" url "fail/parse"
                return False
            Right (Just c) -> do
                logScrape "ceq" url "ok"
                insertCeq conn c
                return True
    return $ any id res

tryPerformScrapeCeq :: Text -> IO (Either HttpException (Maybe Ceq))
tryPerformScrapeCeq = try . scrapeCeq

logScrape :: Text -> Text -> Text -> IO ()
logScrape t url stat = TIO.putStrLn $ "[{}] {} -> {}" `format` (t, url, stat)

periodPerms = do
    y <- reverse [fromYear..toYear]
    s <- [HT, VT]
    p <- [1, 2]
    return $ Period (fromIntegral y) s (fromIntegral p)
