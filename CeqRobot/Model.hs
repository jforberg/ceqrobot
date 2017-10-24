module CeqRobot.Model
( CourseLevel(..)
, CoursePeriod(..)
, Course(..)
, ProgrammeYear(..)
, Ceq(..)
)
where

import Data.Text (Text)

data CourseLevel = LevelG1 | LevelG2 | LevelA

instance Show CourseLevel where
    show LevelG1 = "G1"
    show LevelG2 = "G2"
    show LevelA = "A"

data ProgrammeYear = Year1 | Year2 | Year3 | Year4 | Year5
    deriving (Show, Eq, Ord, Enum)

data CoursePeriod = Lp Bool Bool Bool Bool | Periodical
    deriving (Eq)

instance Show CoursePeriod where
    show Periodical = "Periodical"
    show (Lp lp1 lp2 lp3 lp4) = "Lp" ++ f 1 lp1 ++ f 2 lp2 ++ f 3 lp3 ++ f 4 lp4
        where f _ False = ""
              f n True = show n

data Course = Course
   { courseCode :: Text
   , courseCredits :: Float
   , courseLevel :: CourseLevel
   , courseName :: Text
   , courseComment :: Text
   , courseYear :: Maybe ProgrammeYear
   , coursePeriod :: CoursePeriod
   }
   deriving (Show)

data Ceq = Ceq
   { ceqCourseCode :: Text
   , ceqPeriod :: Text
   , ceqUrl :: Text
   , ceqStudentsReg :: Int
   , ceqStudentsPass :: Int
   , ceqStudentsResp :: Int
   , ceqRelevancyMean :: Int
   , ceqRelevancyStddev :: Int
   , ceqSatisfactionMean :: Int
   , ceqSatisfactionStddev :: Int
   }
   deriving (Show)
