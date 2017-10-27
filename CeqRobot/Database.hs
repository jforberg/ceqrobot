{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CeqRobot.Database
( getConn
, insertCourse
, insertCourseRelation
, insertMasters
, insertCeq
, queueScrape
, peekQueueScrape
, dequeueScrape
)
where

import Control.Monad
import Data.Int
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Database.PostgreSQL.Typed

import CeqRobot.Model

dbConf = defaultPGDatabase
    { pgDBName = "ceqrobot"
    }

getConn :: IO PGConnection
getConn = pgConnect dbConf

insertCourse :: PGConnection -> Course -> IO ()
insertCourse conn course =
    let c = courseCode course
        r = courseCredits course
        l = show $ courseLevel course
        n = courseName course
    in
        void $ pgExecute conn [pgSQL|
            insert into course( code
                              , credits
                              , level
                              , name
                              )
            values (${c}, ${r}, ${l}, ${n})
            on conflict (code) do update
            set code = ${c}
              , credits = ${r}
              , level = ${l}
              , name = ${n}
              , updated = now()
        |]

insertCourseRelation :: PGConnection -> CourseRelation -> IO ()
insertCourseRelation conn cr =
    let c = courseRelCode cr
        p = courseRelProgramme cr
        t = show $ courseRelType cr
        m = courseRelMasters cr
        co = courseRelComment cr
        y = courseRelYear cr
        (pd, lp1, lp2, lp3, lp4) = encPeriod $ courseRelPeriod cr
    in
        void $ pgExecute conn [pgSQL|
            insert into course_relation( code
                                       , programme
                                       , type
                                       , masters
                                       , comment
                                       , year
                                       , periodical
                                       , lp1
                                       , lp2
                                       , lp3
                                       , lp4
                                       )
            values (${c}, ${p}, ${t}, ${m}, ${co}, ${y},
                    ${pd}, ${lp1}, ${lp2}, ${lp3}, ${lp4})
            on conflict (code, programme, masters) do update
            set code = ${c}
              , programme = ${p}
              , type = ${t}
              , masters = ${m}
              , comment = ${co}
              , year = ${y}
              , periodical = ${pd}
              , lp1 = ${lp1}
              , lp2 = ${lp2}
              , lp3 = ${lp3}
              , lp4 = ${lp4}
              , updated = now()
        |]

encPeriod Periodical = (True, False, False, False, False)
encPeriod (Lp x1 x2 x3 x4) = (False, x1, x2, x3, x4)

insertMasters :: PGConnection -> Masters -> IO ()
insertMasters conn mast =
    let Masters p c n = mast
    in void $ pgExecute conn [pgSQL|
        insert into masters(programme, code, name)
        values (${p}, ${c}, ${n})
        on conflict (programme, code) do update
        set programme = ${p}
          , code = ${c}
          , name = ${n}
          , updated = now()
    |]

insertCeq :: PGConnection -> Ceq -> IO ()
insertCeq conn ceq =
    let c = ceqCourseCode ceq
        Period y s_ p = ceqPeriod ceq
        s = show s_
        u = ceqUrl ceq
        reg = ceqRegistered ceq
        pas = ceqPassed ceq
        res = ceqResponded ceq
        qua = ceqQuality ceq
        goa = ceqGoals ceq
        und = ceqUnderstanding ceq
        wor = ceqWorkload ceq
        rel = ceqRelevance ceq
        sat = ceqSatisfaction ceq
    in
        void $ pgExecute conn [pgSQL|
            insert into ceq( code
                           , year
                           , semester
                           , period
                           , url
                           , registered
                           , passed
                           , responded
                           , quality
                           , goals
                           , understanding
                           , workload
                           , relevance
                           , satisfaction
                           )
            values (${c}, ${y}, ${s}, ${p}, ${u},
                    ${reg}, ${pas}, ${res},
                    ${qua}, ${goa}, ${und},
                    ${wor}, ${rel}, ${sat})
            on conflict (code, year, semester, period) do update
            set code = ${c}
              , year = ${y}
              , semester = ${s}
              , period = ${p}
              , url = ${u}
              , registered = ${reg}
              , passed = ${pas}
              , responded = ${res}
              , quality = ${qua}
              , goals = ${goa}
              , understanding = ${und}
              , workload = ${wor}
              , relevance = ${rel}
              , satisfaction = ${sat}
              , updated = now()
        |]

queueScrape :: PGConnection -> Text -> Text -> IO ()
queueScrape conn t ref = void $ pgExecute conn [pgSQL|
        insert into queue(type, ref)
        values (${t}, ${ref})
        on conflict (type, ref) do update
        set prio = random(), updated = now()
    |]

peekQueueScrape :: PGConnection -> IO (Maybe (Text, Text))
peekQueueScrape conn = listToMaybe <$> pgQuery conn [pgSQL|
        select type, ref
        from queue
        order by prio
        limit 1
    |]

dequeueScrape :: PGConnection -> Text -> Text -> IO ()
dequeueScrape conn t ref = void $ pgExecute conn [pgSQL|
        delete from queue
        where type = ${t} and ref = ${ref}
    |]

