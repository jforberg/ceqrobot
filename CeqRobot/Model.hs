module CeqRobot.Model
( CourseLevel(..)
, CoursePeriod(..)
, Semester(..)
, Period(..)
, CourseType(..)
, Course(..)
, CourseRelation(..)
, Ceq(..)
, Masters(..)
)
where

import Data.Int
import Data.Text (Text)

data CourseLevel = LevelG1 | LevelG2 | LevelA

instance Show CourseLevel where
    show LevelG1 = "G1"
    show LevelG2 = "G2"
    show LevelA = "A"

data CoursePeriod = Lp Bool Bool Bool Bool | Periodical
    deriving (Eq)

instance Show CoursePeriod where
    show Periodical = "Periodical"
    show (Lp lp1 lp2 lp3 lp4) = "Lp" ++ f 1 lp1 ++ f 2 lp2 ++ f 3 lp3 ++ f 4 lp4
        where f _ False = ""
              f n True = show n

data Semester = HT | VT
    deriving (Show, Eq)

data Period = Period Int32 Semester Int32
    deriving (Eq)

instance Show Period where
    show (Period y s p) = show y ++ " " ++ show s ++ show p

data CourseType = Obligatory | AltObligatory | Elective
    deriving (Show, Eq)

data Course = Course
    { courseCode :: Text
    , courseCredits :: Double
    , courseLevel :: CourseLevel
    , courseName :: Text
    }
    deriving (Show)

data CourseRelation = CourseRelation
    { courseRelCode :: Text
    , courseRelProgramme :: Text
    , courseRelType :: CourseType
    , courseRelMasters :: Text
    , courseRelComment :: Text
    , courseRelYear :: Int32
    , courseRelPeriod :: CoursePeriod
    }
    deriving (Show)

data Ceq = Ceq
    { ceqCourseCode :: Text
    , ceqPeriod :: Period
    , ceqUrl :: Text
    -- "Antal registrerade på kursen"
    , ceqRegistered :: Maybe Int32
    -- "Antal godkända"
    , ceqPassed :: Maybe Int32
    -- "Antal enkätsvar"
    , ceqResponded :: Maybe Int32
    -- "God undervisning"
    , ceqQuality :: Maybe Int32
    -- "Tydliga Mål"
    , ceqGoals :: Maybe Int32
    -- "Förståelseinriktad examination"
    , ceqUnderstanding :: Maybe Int32
    -- "Lämplig arbetsbelastning"
    , ceqWorkload :: Maybe Int32
    -- "Kursen känns angelägen för min utbildning"
    , ceqRelevance :: Maybe Int32
    -- "Överlag är jag nöjd med kursen"
    , ceqSatisfaction :: Maybe Int32
    }
    deriving (Show)

data Masters = Masters
    { mastersProgramme :: Text
    , mastersCode :: Text
    , mastersName :: Text
    }
    deriving (Show)
