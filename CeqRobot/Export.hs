{-# LANGUAGE OverloadedStrings #-}

module CeqRobot.Export
( PreDatabase(..)
, buildDatabase
, exportData
)
where

import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import qualified Data.Text as T
import Data.Text (Text)

import CeqRobot.Model

data PreDatabase = PreDatabase
    { pdbProgrammes :: [Programme]
    , pdbCourses :: [Course]
    , pdbCourseRelations :: [CourseRelation]
    , pdbMasters :: [Masters]
    , pdbCeqs :: [Ceq]
    , pdbAliases :: [(Text, Text)]
    }
    deriving (Show)

data Database = Database
    { dbProgMap :: Map Text Programme
    , dbCourseMap :: Map Text Course
    , dbCourseCeqMap :: Map Text [Ceq]
    , dbCourseAliasMap :: Map Text [Text]
    , dbProgMastersMap :: Map Text (Map Text Masters)
    , dbProgMastersCourseRelMap :: Map Text (Map Text [CourseRelation])
    }
    deriving (Show)

instance ToJSON Database where
    toJSON (Database pm cm qm am mm lm) =
        object [ "programmes" .= pm
               , "courses" .= cm
               , "ceqs" .= qm
               , "aliases" .= am
               , "masters" .= mm
               , "lotMap" .= lm
               ]

instance ToJSON Programme where
    toJSON p =
        object [ "code" .= programmeCode p
               , "name" .= programmeName p
               ]

instance ToJSON Course where
    toJSON c =
        object [ "code" .= courseCode c
               , "credits" .= courseCredits c
               , "level" .= courseLevel c
               , "name" .= courseName c
               ]

instance ToJSON CourseRelation where
    toJSON r =
        object [ "code" .= courseRelCode r
               , "programme" .= courseRelProgramme r
               , "type" .= courseRelType r
               , "masters" .= courseRelMasters r
               , "comment" .= courseRelComment r
               , "year" .= courseRelProgYear r
               , "period" .= courseRelPeriod r
               ]

instance ToJSON Ceq where
    toJSON q =
        object [ "code" .= ceqCourseCode q
               , "ceqPeriod" .= ceqPeriod q
               , "url" .= ceqUrl q
               , "registered" .= ceqRegistered q
               , "passed" .= ceqPassed q
               , "responded" .= ceqResponded q
               , "quality" .= ceqQuality q
               , "goals" .= ceqGoals q
               , "understanding" .= ceqUnderstanding q
               , "workload" .= ceqWorkload q
               , "relevance" .= ceqRelevance q
               , "satisfaction" .= ceqSatisfaction q
               ]

instance ToJSON Masters where
    toJSON m =
        object [ "code" .= mastersCode m
               , "programme" .= mastersProgramme m
               , "name" .= mastersName m
               ]

instance ToJSON CourseLevel where
    toJSON = jsonShow

instance ToJSON CourseType where
    toJSON = jsonShow

instance ToJSON Semester where
    toJSON = jsonShow

instance ToJSON CoursePeriod where
    toJSON (Lp lp1 lp2 lp3 lp4) =
        object [ "periodical" .= False
               , "lp" .= [lp1, lp2, lp3, lp4]
               ]
    toJSON Periodical =
        object [ "periodical" .= True
               , "lp" .= [False, False, False, False]
               ]

instance ToJSON Period where
    toJSON (Period y s p) = toJSON [toJSON y, toJSON s, toJSON p]

exportData :: PreDatabase -> ByteString
exportData = encode . buildDatabase

buildDatabase :: PreDatabase -> Database
buildDatabase (PreDatabase ps cs rs ms qs as) = Database pmap cmap qmap amap mmap lmap
    where pmap = collectSingle programmeCode ps

          cmap = collectSingle courseCode cs

          qmap = collectMany ceqCourseCode qs

          amap = M.fromListWith (++) . concat $ [ [(a1, [a2]), (a2, [a1])] | (a1, a2) <- as]

          mmap = M.map (collectSingle mastersCode)  mmap'
          mmap' = collectMany mastersProgramme ms

          lmap = M.map (collectMany courseRelMasters) lmap'
          lmap' = collectMany courseRelProgramme rs

          collectMany keyf vs = M.fromListWith (++) [(keyf v, [v]) | v <- vs]

          collectSingle keyf vs = M.fromList [(keyf v, v) | v <- vs]

jsonShow :: Show a => a -> Value
jsonShow = toJSON . T.toLower . T.pack . show
