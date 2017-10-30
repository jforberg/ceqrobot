{-# LANGUAGE OverloadedStrings #-}

module CeqRobot.Export
( exportData
)
where

import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Data.Function
import Data.List
import qualified Data.Map as M
import Data.Map (Map)
import qualified Data.Text as T
import Data.Text (Text)

import CeqRobot.Model

import CeqRobot.Database

data CourseInfo = CourseInfo
    { ciCourse :: Course
    , ciRelation :: CourseRelation
    }
    deriving (Show)

data Database = Database
    { dbMasters :: [Masters]
    , dbCourses :: [CourseInfo]
    , dbMasterCourseMap :: Map Text [Text]
    , dbCourseCeqMap :: Map Text [Ceq]
    , dbCourseAliasMap :: Map Text [Text]
    }
    deriving (Show)

instance ToJSON Database where
    toJSON (Database ms cs mcm ccm cam) =
        object [ "masters" .= map masters ms
               , "courses" .= object (map course cs)
               , "ceqs" .= object (map ceqs cs)
               , "aliases" .= cam
               ]
        where masters m =
                  object [ "code" .= mastersCode m
                         , "name" .= mastersName m
                         , "courses" .= M.findWithDefault [] (mastersCode m) mcm
                         ]

              course ci@(CourseInfo c r) = (courseCode c, toJSON ci)

              ceqs (CourseInfo c r) =
                  ( courseCode c
                  , toJSON . sortBy (compare `on` ceqPeriod) $ (M.findWithDefault [] (courseCode c) ccm)
                  )

instance ToJSON CourseInfo where
    toJSON (CourseInfo c r) =
        object [ "credits" .= courseCredits c
               , "level" .= courseLevel c
               , "name" .= courseName c
               , "type" .= courseRelType r
               , "masters" .= courseRelMasters r
               , "comment" .= courseRelComment r
               , "year" .= courseRelYear r
               , "period" .= courseRelPeriod r
               ]

instance ToJSON Ceq where
    toJSON c =
        object [ "ceqPeriod" .= ceqPeriod c
               , "url" .= ceqUrl c
               , "registered" .= ceqRegistered c
               , "passed" .= ceqPassed c
               , "responded" .= ceqResponded c
               , "quality" .= ceqQuality c
               , "goals" .= ceqGoals c
               , "understanding" .= ceqUnderstanding c
               , "workload" .= ceqWorkload c
               , "relevance" .= ceqRelevance c
               , "satisfaction" .= ceqSatisfaction c
               ]

instance ToJSON CourseLevel where
    toJSON = jsonShow

instance ToJSON CourseType where
    toJSON = jsonShow

instance ToJSON CoursePeriod where
    toJSON Periodical =
        object [ "periodical" .= True
               , "lp" .= [False, False, False, False]
               ]
    toJSON (Lp lp1 lp2 lp3 lp4) =
        object [ "periodical" .= False
               , "lp" .= [lp1, lp2, lp3, lp4]
               ]

instance ToJSON Period where
    toJSON (Period y s p) = toJSON [toJSON y, toJSON s, toJSON p]

instance ToJSON Semester where
    toJSON = jsonShow

exportData :: [Masters] -> [(Course, CourseRelation)] -> [Ceq] -> [(Text, Text)] -> ByteString
exportData ms cs_crs cqs cas = encode $ buildDatabase ms cs_crs cqs cas

buildDatabase ms cs_crs cqs cas =
    Database { dbMasters = ms
             , dbCourses = map (uncurry CourseInfo) cs_crs
             , dbMasterCourseMap = mcm
             , dbCourseCeqMap = ccm
             , dbCourseAliasMap = cam
             }
        where mcm = foldr f M.empty cs_crs
              f (c, r) = M.insertWith (++) (courseRelMasters r) [courseCode c]

              ccm = foldr g M.empty cqs
              g c = M.insertWith (++) (ceqCourseCode c) [c]

              cam = foldr h M.empty cas
              h (c1, c2) = M.insertWith (++) c1 [c2] .
                           M.insertWith (++) c2 [c1]

jsonShow :: Show a => a -> Value
jsonShow = toJSON . T.toLower . T.pack . show
