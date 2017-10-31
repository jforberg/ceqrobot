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

    cs <- loadCourses conn
    rs <- loadCourseRelations conn
    ms <- loadMasters conn
    qs <- loadCeqs conn
    as <- loadCourseAliases conn

    LBS.putStr "var db = "
    LBS.putStr . exportData $ (PreDatabase cs rs ms qs as)
