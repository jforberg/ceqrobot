module CeqRobot.Course
( CourseLevel(..)
, Course(..)
)
where

import Data.Text (Text)

data CourseLevel = LevelG1 | LevelG2 | LevelA
    deriving (Show)

data ProgrammeYear = Year1 | Year2 | Year3 | Year4 | Year5
    deriving (Show, Eq, Ord, Enum)

data Course = Course
   { courseCode :: Text
   , courseCredits :: Double
   , courseLevel :: CourseLevel
   , courseName :: Text
   , courseComment :: Text
   , courseYear :: Maybe ProgrammeYear
   }
   deriving (Show)
