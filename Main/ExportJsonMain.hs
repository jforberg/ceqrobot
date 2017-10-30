{-# LANGUAGE OverloadedStrings #-}

module Main
( main
)
where

import qualified Data.ByteString.Lazy as LBS

import CeqRobot.Database
import CeqRobot.Export

main = do
    conn <- getConn

    ms <- loadMasters conn
    cs_crs <- loadCoursesAndRelations conn
    cqs <- loadCeqs conn
    cas <- loadCourseAliases conn

    LBS.putStr "var db = "
    LBS.putStr $ exportData ms cs_crs cqs cas
