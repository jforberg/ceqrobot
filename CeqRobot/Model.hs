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
    deriving (Eq, Show, Read)

data CoursePeriod = Lp Bool Bool Bool Bool | Periodical
    deriving (Eq, Show)

data Semester = VT | HT
    deriving (Eq, Ord, Show, Read)

data Period = Period Int32 Semester Int32
    deriving (Eq, Show)

instance Ord Period where
    (Period y1 s1 p1) `compare` (Period y2 s2 p2) =
        (y1, s1, p1) `compare` (y2, s2, p2)

data CourseType = Obligatory | AltObligatory | Elective
    deriving (Eq, Show, Read)

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
