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

    ps <- loadProgrammes conn
    cs <- loadCourses conn
    rs <- loadCourseRelations conn
    ms <- loadMasters conn
    qs <- loadCeqs conn
    as <- loadCourseAliases conn

    LBS.putStr "var db = "
    LBS.putStr . exportData $ (PreDatabase ps cs rs ms qs as)
