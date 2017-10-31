{-# LANGUAGE OverloadedStrings #-}

module CeqRobot.Export
( PreDatabase(..)
, buildDatabase
, exportData
)
where

import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Data.Function
import Data.List
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import qualified Data.Text as T
import Data.Text (Text)

import CeqRobot.Model

import CeqRobot.Database -- DEBUG

data PreDatabase = PreDatabase
    { pdbCourses :: [Course]
    , pdbCourseRelations :: [CourseRelation]
    , pdbMasters :: [Masters]
    , pdbCeqs :: [Ceq]
    , pdbAliases :: [(Text, Text)]
    }
    deriving (Show)

data Database = Database
    { dbProgrammeMap :: Map Text [CourseRelation]
      -- programme -> masters (or empty str) -> course rels
    , dbMastersMap :: Map Text (Map Text [CourseRelation])
    , dbCourseMap :: Map Text Course
    , dbCourseCeqMap :: Map Text [Ceq]
    , dbCourseAliasMap :: Map Text [Text]
    }
    deriving (Show)

instance ToJSON Database where
    toJSON (Database pm mm cm qm am) =
        object [ "programmes" .= pm
               , "programmes_masters" .= mm
               , "courses" .= cm
               , "ceqs" .= qm
               , "aliases" .= am
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
               , "year" .= courseRelYear r
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
buildDatabase (PreDatabase cs rs ms qs as) =
    Database { dbProgrammeMap = pmap
             , dbMastersMap = mmap
             , dbCourseMap = cmap
             , dbCourseCeqMap = qmap
             , dbCourseAliasMap = amap
             }
        where mmap = M.map f pmap
              f rs' = M.fromListWith (++) [(courseRelMasters r, [r]) | r <- rs']

              pmap = M.fromListWith (++) [(courseRelProgramme r, [r]) | r <- rs]

              cmap = M.fromList [(courseCode c, c) | c <- cs]

              qmap = M.fromListWith (++) [(ceqCourseCode q, [q]) | q <- qs]

              amap = M.fromListWith (++) . concat $ [ [(a1, [a2]), (a2, [a1])] | (a1, a2) <- as]

jsonShow :: Show a => a -> Value
jsonShow = toJSON . T.toLower . T.pack . show
