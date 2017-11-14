{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CeqRobot.Database
( DB
, getConn
, insertProgramme
, loadProgrammes
, insertCourse
, loadCourses
, loadCourseRelations
, insertCourseRelation
, deleteCourseRelations
, insertMasters
, loadMasters
, insertCeq
, loadCeqs
, loadCourseAliases
, queueScrape
, peekQueueScrape
, dequeueScrape
)
where

import Control.Monad
import Data.Maybe
import Data.Text (Text)
import Data.Tuple.Curry
import Database.PostgreSQL.Typed

import CeqRobot.Model

type DB = PGConnection

dbConf :: PGDatabase
dbConf = defaultPGDatabase
    { pgDBName = "ceqrobot"
    }

getConn :: IO PGConnection
getConn = pgConnect dbConf

insertProgramme :: PGConnection -> Programme -> IO ()
insertProgramme conn (Programme c n) = void $ pgExecute conn [pgSQL|
        insert into programme( code
                             , name
                             )
        values (${c}, ${n})
        on conflict (code) do update
        set code = ${c}
          , name = ${n}
    |]

loadProgrammes :: PGConnection -> IO [Programme]
loadProgrammes conn = map (uncurry Programme) <$> pgQuery conn [pgSQL|
        select code, name
        from programme
    |]

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
        py = courseRelProgYear cr
        vy = courseRelValidYear cr
        (pd, lp1, lp2, lp3, lp4) = encPeriod $ courseRelPeriod cr
    in
        void $ pgExecute conn [pgSQL|
            insert into course_relation( code
                                       , programme
                                       , type
                                       , masters
                                       , comment
                                       , programmeYear
                                       , validYear
                                       , periodical
                                       , lp1
                                       , lp2
                                       , lp3
                                       , lp4
                                       )
            values (${c}, ${p}, ${t}, ${m}, ${co}, ${py}, ${vy},
                    ${pd}, ${lp1}, ${lp2}, ${lp3}, ${lp4})
            on conflict (code, programme, masters, validYear) do update
            set code = ${c}
              , programme = ${p}
              , type = ${t}
              , masters = ${m}
              , comment = ${co}
              , programmeYear = ${py}
              , validYear = ${vy}
              , periodical = ${pd}
              , lp1 = ${lp1}
              , lp2 = ${lp2}
              , lp3 = ${lp3}
              , lp4 = ${lp4}
              , updated = now()
        |]

    where encPeriod Periodical = (True, False, False, False, False)
          encPeriod (Lp x1 x2 x3 x4) = (False, x1, x2, x3, x4)

deleteCourseRelations :: PGConnection -> IO ()
deleteCourseRelations conn = void $ pgExecute conn [pgSQL|
        delete from course_relation
    |]

loadCourses :: PGConnection -> IO [Course]
loadCourses conn =
    map f <$> pgQuery conn [pgSQL|
        select c.code
             , c.credits
             , c.level
             , c.name
        from course c
    |]
    where f (co, cr, le, na) =
              Course { courseCode = co
                     , courseCredits = cr
                     , courseLevel = read le
                     , courseName = na
                     }

loadCourseRelations :: PGConnection -> IO [CourseRelation]
loadCourseRelations conn = map f <$> pgQuery conn [pgSQL|
        select r.code
             , r.programme
             , r.type
             , r.masters
             , r.comment
             , r.programmeYear
             , r.validYear
             , r.periodical
             , r.lp1
             , r.lp2
             , r.lp3
             , r.lp4
        from course_relation r
        where r.validYear = (select max(validYear) from course_relation)
    |]
    where f (co, pr, ty, ma, cm, py, vy, pe, l1, l2, l3, l4) =
              CourseRelation { courseRelCode = co
                             , courseRelProgramme = pr
                             , courseRelType = read ty
                             , courseRelMasters = ma
                             , courseRelComment = cm
                             , courseRelProgYear = py
                             , courseRelValidYear = vy
                             , courseRelPeriod = if pe then Periodical
                                                       else Lp l1 l2 l3 l4
                             }

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

loadMasters :: PGConnection -> IO [Masters]
loadMasters conn = do
    rs <- pgQuery conn [pgSQL|
        select programme
             , code
             , name
        from masters
    |]

    return . map (uncurryN Masters) $ rs

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

loadCeqs :: PGConnection -> IO [Ceq]
loadCeqs conn = do
    rs <- pgQuery conn [pgSQL|
        select code
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
        from ceq
    |]

    return $ map f rs
        where f (co, ye, se, pe, ur, re, pa, rs, qu, go, un, wo, rl, sa) =
                  Ceq { ceqCourseCode = co
                      , ceqPeriod = Period ye (read se) pe
                      , ceqUrl = ur
                      , ceqRegistered = re
                      , ceqPassed = pa
                      , ceqResponded = rs
                      , ceqQuality = qu
                      , ceqGoals = go
                      , ceqUnderstanding = un
                      , ceqWorkload = wo
                      , ceqRelevance = rl
                      , ceqSatisfaction = sa
                      }

loadCourseAliases :: PGConnection -> IO [(Text, Text)]
loadCourseAliases conn = mapMaybe f <$> pgQuery conn [pgSQL|
        select distinct greatest(c1.code, c2.code)
                      , least(c1.code, c2.code)
        from course c1, course c2
        where c1.name = c2.name and c1.code != c2.code
    |]
        where f (Nothing, _) = Nothing
              f (_, Nothing) = Nothing
              f (Just a, Just b) = Just (a, b)

queueScrape :: PGConnection -> Double -> ScrapeJob -> IO ()
queueScrape conn basePrio (ScrapeJob t ref meta) =
    void $ pgExecute conn [pgSQL|
        insert into queue(type, ref, meta, prio)
        values (${t}, ${ref}, ${meta}, ${basePrio} + random())
        on conflict (type, ref) do update
        set meta = ${meta}
          , prio = ${basePrio} + random()
          , updated = now()
    |]

peekQueueScrape :: PGConnection -> IO (Maybe ScrapeJob)
peekQueueScrape conn = listToMaybe . map (uncurryN ScrapeJob) <$> pgQuery conn [pgSQL|
        select type, ref, meta
        from queue
        order by prio asc
        limit 1
    |]

dequeueScrape :: PGConnection -> ScrapeJob -> IO ()
dequeueScrape conn (ScrapeJob t ref _) = void $ pgExecute conn [pgSQL|
        delete from queue
        where type = ${t} and ref = ${ref}
    |]
