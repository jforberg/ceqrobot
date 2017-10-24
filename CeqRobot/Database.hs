{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CeqRobot.Database
( getConn
, insertCourse
)
where

import Control.Monad
import Data.Int
import Database.PostgreSQL.Typed

import CeqRobot.Model

dbConf = defaultPGDatabase
    { pgDBName = "ceqrobot"
    }

insertCourse conn course =
    let c = courseCode course
        r = courseCredits course
        l = show $ courseLevel course
        n = courseName course
        m = courseComment course
        y :: Maybe Int32 = fromIntegral . fromEnum <$> courseYear course
    in
        pgExecute conn [pgSQL|
            insert into course(code, credits, level, name, comment, year)
            values (${c}, ${r}, ${l}, ${n}, ${m}, ${y})
            on conflict (code) do update
            set code = ${c}, credits = ${r}, level = ${l}, name = ${n},
                comment = ${m}, year = ${y}, updated = now()
    |] >> return ()

getConn = pgConnect dbConf

