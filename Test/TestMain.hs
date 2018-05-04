{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text (Text)
import Data.FileEmbed
import Test.Hspec
import Text.HTML.TagSoup (Tag, parseTags)

import CeqRobot.Model
import CeqRobot.Scraper

parseFile :: Text -> [Tag Text]
parseFile = parseTags

main :: IO()
main = hspec $ do
    describe "Scaper" $ do
        describe "parseCeq" parseCeqSpec
        describe "scapeMasters" scrapeMastersSpec

parseCeqSpec = do
    it "parses 'long' example correctly" $
        let input = parseFile $(embedStringFile "Test/ceq-example-long.html")
            output = parseCeq "test.com" input
        in output `shouldBe` Just Ceq
            { ceqCourseCode = "FMA021"
            , ceqPeriod = Period 2017 VT 2
            , ceqUrl = "test.com"
            , ceqRegistered = Just 27
            , ceqPassed = Just 19
            , ceqResponded = Just 10
            , ceqQuality = Just $ -8
            , ceqGoals = Just 33
            , ceqUnderstanding = Just 47
            , ceqWorkload = Just 6
            , ceqRelevance = Just 95
            , ceqSatisfaction = Just 45
            }

    it "parses 'short' example correctly" $
        let input = parseFile $(embedStringFile "Test/ceq-example-short.html")
            output = parseCeq "test.com" input
        in output `shouldBe` Just Ceq
            { ceqCourseCode = "FKFN01"
            , ceqPeriod = Period 2013 VT 1
            , ceqUrl = "test.com"
            , ceqRegistered = Just 9
            , ceqPassed = Just 9
            , ceqResponded = Nothing
            , ceqQuality = Nothing
            , ceqGoals = Nothing
            , ceqUnderstanding = Nothing
            , ceqWorkload = Nothing
            , ceqRelevance = Nothing
            , ceqSatisfaction = Nothing
            }

scrapeMastersSpec = do
    it "parses 'mandatory' example correctly" $
        let input = parseFile $(embedStringFile "Test/courses-example-mandatory.html")
            output = parseCourses "F" 2018 input
        in (take 1 <$> output) `shouldBe` Just
            [ ( Course { courseCode = "FMAB20"
                       , courseCredits = 6.0
                       , courseLevel = LevelG1
                       , courseName = "Linjär algebra"
                       }
              , CourseRelation { courseRelCode = "FMAB20"
                               , courseRelProgramme = "F"
                               , courseRelType = Obligatory
                               , courseRelMasters = ""
                               , courseRelComment = ""
                               , courseRelValidYear = 2018
                               , courseRelProgYear = 1
                               , courseRelPeriod = Lp True False False False
                               }
              )
            ]
